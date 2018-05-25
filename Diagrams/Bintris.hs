{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Diagrams.Bintris (
    diagrams
  ) where
{-
function logMouseMove(event) {
  var e = event || window.event;
  mousePos = { x: e.clientX, y: e.clientY };
  console.log(mousePos);
}
window.onmousemove = logMouseMove;

-}

import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

awidth  (v :: Float) = A.width (toValue v)
aheight (v :: Float) = A.height (toValue v)
ax (v :: Float) = A.x (toValue v)
ay (v :: Float) = A.y (toValue v)

colorTitle :: Float -> Float -> T.Text -> S.AttributeValue -> S.Svg
colorTitle x y text f =
  S.g $ do
    S.rect ! awidth 15 ! aheight 15 ! ax x ! ay y ! A.fill f
    S.text_ ! ax (x+19) ! ay (y+12) ! A.fill "white" $ (S.text text)

titlescreenRasterSvg :: S.Svg
titlescreenRasterSvg =
--  S.docTypeSvg ! A.version "1.1" ! A.width "716" ! A.height "539" ! A.viewbox "0 0 716 539" $ do
  -- image width,height 716x539
  S.docTypeSvg ! A.version "1.1" ! A.width "100%" ! A.height "100%" ! A.viewbox "0 0 716 539" $ do
    S.g $ do
      S.image ! awidth 716 ! aheight 539 ! A.xlinkHref "/images/bintris/titlescreen_for_blog.png"
      pixrect 0 0 320 1 ! A.fill startIrqFill
      pixrect 0 (200-12) 320 1 ! A.fill modeIrqFill
      pixrect 0 (200-8) 320 1 ! A.fill colorIrqFill
    S.g $ do
      let starty = 539 - 40
      colorTitle bmLeft starty "top frame" startIrqFill
      colorTitle (bmLeft+150) starty "mode switch" modeIrqFill
      colorTitle (bmLeft+300) starty "color line" colorIrqFill
  where
    startIrqFill = "#008d46"
    modeIrqFill  = "#cf4600"
    colorIrqFill = "#00468d"
    pixrect :: Float -> Float -> Float -> Float -> S.Svg
    pixrect x y w h =
      let (x', y') = pixelXY x y
          (w', h') = (w/320.0*xw, h/200.0*yh)
      in S.rect ! A.width (toValue w') ! A.height (toValue h') ! ax x' ! ay y'

    bmLeft   = 58
    bmRight  = 657
    bmTop    = 66
    bmBottom = 466
    xw = bmRight - bmLeft
    yh = bmBottom - bmTop
    pixelXY x y = (bmLeft + x/320.0*xw, bmTop + y/200.0*yh)

titlescreenRaster :: String
titlescreenRaster = renderSvg titlescreenRasterSvg

diagrams =
  [ ("bintris_title_svg", titlescreenRaster)
  ]

test :: IO ()
test = do
 let a = renderSvg titlescreenRasterSvg
 let fname = "test.svg"
 putStrLn ("writing svg to " ++ fname)
 writeFile fname a
