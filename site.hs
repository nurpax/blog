{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<|>))
import           Control.Monad (filterM, void)
import           Data.List (elem)
import           Data.Maybe (listToMaybe)
import           Data.Monoid   ((<>), mconcat)
import qualified GHC.IO.Encoding as E

import           Hakyll
import           Hakyll.Core.Metadata
import           Text.Pandoc (
                   Pandoc
                 , Block (..)
                 , Inline(..)
                 , MathType(..)
                 )
import           Text.Pandoc.Walk (walk, query)

import qualified Diagrams.Bintris as Bintris

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

-- | Read a page render using pandoc and apply some substitutions like
-- inserting SVG for diagrams
pandocCompilerXform :: (Pandoc -> Pandoc) -> Compiler (Item String)
pandocCompilerXform f =
    pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        f

substDiagrams :: Pandoc -> Pandoc
substDiagrams doc = walk bintrisSvg doc
  where
    bintrisSvg :: Inline -> Inline
    bintrisSvg e@(Math InlineMath name) =
      foldr (\(n, svg) acc -> if name == n then rawHtml svg else acc) e Bintris.diagrams
    bintrisSvg e = e
    rawHtml html = RawInline "html" html

-- Get rid of ``` {.hakyll-inline-css} blocks in Pandoc so that the inline
-- styles don't get output as <pre> blocks.
removeInlineCss :: Pandoc -> Pandoc
removeInlineCss doc = walk removeCSS doc
  where
    removeCSS block@(CodeBlock (_, classes, _) _) =
      if "hakyll-inline-css" `elem` classes then
        Null
      else
        block
    removeCSS elt = elt

-- Extract CSS fragments from ``` {.hakyll-inline-css} blocks.  The contents
-- of this verbatim block will be inserted into the page <head> as a <style>
-- element.
extractInlineCss :: Item String -> Compiler (Maybe String)
extractInlineCss body = do
  doc <- readPandoc body
  return . listToMaybe . query extractCssBlock $ doc
  where
    extractCssBlock :: Block -> [String]
    extractCssBlock block@(CodeBlock (_, classes, _) b) =
      if "hakyll-inline-css" `elem` classes then
        [b]
      else
        []
    extractCssBlock block = []

buildRules :: Rules ()
buildRules = do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/**" $ do
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
        compile $ do
          body <- getResourceBody
          inlineCss <- extractInlineCss body
          let postCtx' = maybe postCtx (\css -> postCtx <> constField "inline_css" css) inlineCss
          pandocCompilerXform (removeInlineCss . substDiagrams)
             >>= saveSnapshot "content"
             >>= return . fmap demoteHeaders
             >>= loadAndApplyTemplate "templates/post.html" postCtx'
             >>= loadAndApplyTemplate "templates/default.html" postCtx'
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


main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config buildRules

-- For starting with ghcid site.hs --test ghcidEntry
-- forces site recompilation when the .hs source changes.
ghcidEntry :: IO ()
ghcidEntry = do
    void $ hakyllWithExitCodeAndArgs config (Options False Clean) buildRules
    void $ hakyllWithExitCodeAndArgs config (Options False (Watch "127.0.0.1" 8000 False)) buildRules

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
