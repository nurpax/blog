{-# LANGUAGE ScopedTypeVariables #-}

module Diagrams.Bintris.Util (
    (<>)
  , (!)
  , toValue
  , toMarkup
  , renderSvg
  , ax
  , ay
  , awidth
  , aheight
  , showt
) where

import Data.Monoid ((<>))
import Text.Blaze.Svg11 ((!), toValue, toMarkup)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Text as T
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

awidth  (v :: Float) = A.width (toValue v)

aheight (v :: Float) = A.height (toValue v)

ax (v :: Float) = A.x (toValue v)

ay (v :: Float) = A.y (toValue v)

showt :: (Show a) => a -> T.Text
showt = T.pack . show

