{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (filterM)
import           Data.Monoid   (mappend, mconcat)

import           Hakyll
import           Hakyll.Core.Metadata

postCtx :: Context String
postCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

config :: Configuration
config = defaultConfiguration

publicOnly :: (MonadMetadata m, Functor m) => m [Item a] -> m [Item a]
publicOnly i = i >>= \lst -> filterM isPublic lst
  where
    isPublic i = do
      f <- getMetadataField (itemIdentifier i) "public"
      return $ f == Just "true"

main :: IO ()
main = hakyllWith config $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList "posts/*" (publicOnly . recentFirst)
            makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                    (constField "posts" list `mappend` defaultContext)
              >>= loadAndApplyTemplate "templates/default.html"
                    (constField "title" "All posts" `mappend` defaultContext)
              >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postList "posts/*" (fmap (take 10) . publicOnly . recentFirst)
            let ctx = constField "posts" list `mappend`
                      constField "title" "Home" `mappend`
                      defaultContext

            makeItem list
              >>= loadAndApplyTemplate "templates/index.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . publicOnly . recentFirst
                >>= renderAtom feedConfiguration feedCtx

    -- Read templates
    match "templates/*" $ compile templateCompiler

postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl postCtx posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "nurpax.github.com blog"
    , feedDescription = "Notes on programming in Haskell"
    , feedAuthorName  = "Janne Hellsten"
    , feedAuthorEmail = "jjhellst@gmail.com"
    , feedRoot        = "http://nurpax.github.com"
    }
