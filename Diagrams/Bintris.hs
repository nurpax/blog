{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Diagrams.Bintris (
    diagrams
  ) where

import Diagrams.Bintris.Gamescreen (gamescreen)
import Diagrams.Bintris.Titlescreen (titlescreenRaster)

diagrams =
  [ ("bintris_title_svg", titlescreenRaster)
  , ("bintris_gamescreen_svg", gamescreen)
  ]
