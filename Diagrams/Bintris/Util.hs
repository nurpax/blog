{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
  , baseCss
  , makeScale
) where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Text.Blaze.Svg11 ((!), ToValue, toValue, toMarkup)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Data.Text as T
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

data TransformMtx = TransformMtx {
    m11 :: Float
  , m12 :: Float
  , m13 :: Float
  , m21 :: Float
  , m22 :: Float
  , m23 :: Float
} deriving (Show, Eq)

instance ToValue TransformMtx where
  toValue (TransformMtx m11 m12 m13 m21 m22 m23) =
    toValue $ "matrix(" <> (intercalate "," . map show $ [m11, m12, m13, m21, m22, m23]) <> ")"

awidth  (v :: Float) = A.width (toValue v)

aheight (v :: Float) = A.height (toValue v)

ax (v :: Float) = A.x (toValue v)

ay (v :: Float) = A.y (toValue v)

showt :: (Show a) => a -> T.Text
showt = T.pack . show

baseCss :: T.Text
baseCss = "text { font-family: \"arial\";}\n\n"

makeScale :: Float -> (Float, Float) -> TransformMtx
makeScale scl (ox, oy) = TransformMtx scl 0 0 scl (ox-scl*ox) (oy-scl*oy)

