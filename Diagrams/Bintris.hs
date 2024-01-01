{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Diagrams.Bintris (
    diagrams
  ) where

import qualified Data.Text as T

import Diagrams.Bintris.Gamescreen (gamescreen)
import Diagrams.Bintris.Titlescreen (titlescreenRaster)

diagrams :: [(T.Text, T.Text)]
diagrams =
  [ ("bintris_title_svg", titlescreenRaster)
  , ("bintris_gamescreen_svg", gamescreen)
  ]
