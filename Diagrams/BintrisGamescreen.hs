{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Diagrams.BintrisGamescreen (
    gamescreen
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), toValue, toMarkup)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

awidth  (v :: Float) = A.width (toValue v)

aheight (v :: Float) = A.height (toValue v)

ax (v :: Float) = A.x (toValue v)

ay (v :: Float) = A.y (toValue v)

-- C64 screen position in pixels within the .png
bmLeft   = 32
bmRight  = bmLeft+320
bmTop    = 36
bmBottom = 200+bmTop
xw = bmRight - bmLeft
yh = bmBottom - bmTop
-- Scale from 320x200 pixel resolution to svg image coordinates
pixelY y = bmTop + y/200.0*yh
pixelXY x y = (bmLeft + x/320.0*xw, pixelY y)

showt :: (Show a) => a -> T.Text
showt = T.pack . show

pixrect :: Float -> Float -> Float -> Float -> S.Svg
pixrect x y w h =
  let (x', y') = pixelXY x y
      (w', h') = (w/320.0*xw, h/200.0*yh)
  in S.rect ! A.width (toValue w') ! A.height (toValue h') ! ax x' ! ay y'

css :: T.Text
css = ".pointsample { image-rendering: pixelated; }"

-- render a sprite borders assuming the sprite is not 2x scaled in either
-- direction.
spriteBorder :: Float -> Float -> S.Svg
spriteBorder x y = do
  pixrect x y 24 1
  pixrect (x+24) y 1 21
  pixrect x (y+21) 24 1
  pixrect x y 1 21

gamescreenSvg :: S.Svg
gamescreenSvg =
  S.docTypeSvg ! A.version "1.1" ! A.width "100%" ! A.height "100%" ! A.viewbox "0 0 384 272" $ do
    S.style $ toMarkup css
    S.g $ do
      S.image ! awidth 384 ! aheight 272 ! A.class_ "pointsample" ! A.xlinkHref "/images/bintris/gamescreen-collapse.gif"
      -- 0/1 sprite
      spriteBorder 40 (122-6*8) ! A.fill sprite1color
      -- Collapse sprites
      mapM_ (\x -> spriteBorder x 122 ! A.fill sprite1color) [x*24 + 40 | x <- [0..2]]
      -- Score
      mapM_ (\x -> spriteBorder x 32 ! A.fill sprite1color) [x*24 + 126 | x <- [0..2]]
      -- Bintris logo
      mapM_ (\x -> spriteBorder x 0 ! A.fill sprite0color) [x*24 + 40 | x <- [0..3]]
  where
    sprite0color = "#44ff22"
    sprite1color = "#ff4422"

gamescreen :: String
gamescreen = renderSvg gamescreenSvg

test :: IO ()
test = do
 let fname = "test.svg"
 putStrLn ("writing svg to " ++ fname)
 writeFile fname gamescreen
