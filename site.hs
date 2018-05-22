{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (filterM)
import           Data.Monoid   ((<>), mconcat)
import qualified GHC.IO.Encoding as E

import           Hakyll
import           Hakyll.Core.Metadata

seriesLinks :: Context String
seriesLinks =
  functionField "seriesLinks" $ \args item -> do
    seriesTag <- getMetadataField (itemIdentifier item) "series"
    case seriesTag of
      Just tag -> do
        let fname = fromFilePath ("templates/series-" <> tag <> ".html")
        itemBody <$> loadAndApplyTemplate fname defaultContext item
      Nothing -> return ""

postCtx :: Context String
postCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , seriesLinks
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
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
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
                    (constField "posts" list <> defaultContext)
              >>= loadAndApplyTemplate "templates/default.html"
                    (constField "title" "All posts" <> defaultContext)
              >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postList "posts/*" (fmap (take 10) . publicOnly . recentFirst)
            let ctx = constField "posts" list <>
                      constField "title" "Home" <>
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
