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

colorTitle :: Float -> Float -> T.Text -> S.AttributeValue -> S.Svg
colorTitle x y text f =
  S.g $ do
    S.rect ! awidth 15 ! aheight 15 ! ax x ! ay y ! A.fill f
    S.text_ ! ax (x+19) ! ay (y+12) ! A.fill "white" $ (S.text text)

-- C64 screen position in pixels within the .png
bmLeft   = 58
bmRight  = 657
bmTop    = 66
bmBottom = 466
xw = bmRight - bmLeft
yh = bmBottom - bmTop
-- Scale from 320x200 pixel resolution to svg image coordinates
pixelY y = bmTop + y/200.0*yh
pixelXY x y = (bmLeft + x/320.0*xw, pixelY y)

beamStartLine = -20
startLine = -5
modeswitchLine = 200-12
colorLine = 200-8

showt :: (Show a) => a -> T.Text
showt = T.pack . show

animCss :: T.Text
animCss =
  clines [ "#rasterbeam {"
         , "  animation-name: rasterline;"
         , "  animation-duration: 4s;"
         , "  animation-iteration-count: infinite;"
         , "  animation-timing-function: linear;"
         , "}"
         , "@keyframes rasterline {"
         , "  from { transform: translateY(" <> showt (pixelY beamStartLine) <> "px); }"
         , "  to { transform: translateY(" <> showt (pixelY transYLines) <> "px);  }"
         , "}"
         , makeHighlightAnim "hilite1" startLine
         , makeHighlightAnim "hilite2" modeswitchLine
         , makeHighlightAnim "hilite3" colorLine
         ]
  where
    pixYPerc y = (y - beamStartLine) / (transYLines - beamStartLine) * 100.0
    clines = T.concat . map (\s -> T.append s "\n")
    transYLines = 220
    makeHighlightAnim id_ pixY =
      let yp0 = showt $ pixYPerc pixY
          yp1 = showt $ (pixYPerc pixY)+0.01
          yp2 = showt $ (pixYPerc pixY)+10 in
      clines [ "#"<> id_ <> " {"
              , "  animation-name: " <> id_ <> ";"
              , "  animation-duration: 4s;"
              , "  animation-iteration-count: infinite;"
              , "  animation-timing-function: linear;"
              , "  transform-origin: center;"
              , "  transform-box: fill-box;"
              , "}"
              , "@keyframes " <> id_ <> " {"
              , "  0% { transform:scale(1,1); }"
              , "  " <> yp0 <> "% { transform:scale(1,1); }"
              , "  " <> yp1 <> "% { transform:scale(1.5,1.5); }"
              , "  " <> yp2 <> "% { transform:scale(1,1);}"
              , "}"
              ]

titlescreenRasterSvg :: S.Svg
titlescreenRasterSvg =
--  S.docTypeSvg ! A.version "1.1" ! A.width "716" ! A.height "539" ! A.viewbox "0 0 716 539" $ do
  -- image width,height 716x539
  S.docTypeSvg ! A.version "1.1" ! A.width "100%" ! A.height "100%" ! A.viewbox "0 0 716 539" $ do
    S.style $ toMarkup animCss
    S.g $ do
      S.image ! awidth 716 ! aheight 539 ! A.xlinkHref "/images/bintris/titlescreen_for_blog.png"
      pixrect (-40) startLine 400 1 ! A.fill startIrqFill
      pixrect (-40) modeswitchLine 400 1 ! A.fill modeIrqFill
      pixrect (-40) colorLine 400 1 ! A.fill colorIrqFill
      pixrect (-40) (-33) 400 1 ! A.id_ "rasterbeam" ! A.fill "#ffffff" ! A.opacity "0.7"
    S.g $ do
      let starty = 539 - 30
      let tx = bmLeft + 95
      colorTitle tx starty "top frame" startIrqFill ! A.id_ "hilite1"
      colorTitle (tx+1*140) starty "mode switch" modeIrqFill ! A.id_ "hilite2"
      colorTitle (tx+2*140) starty "color line" colorIrqFill ! A.id_ "hilite3"
  where
    startIrqFill = "#008d46"
    modeIrqFill  = "#cf4600"
    colorIrqFill = "#0046cf"
    pixrect :: Float -> Float -> Float -> Float -> S.Svg
    pixrect x y w h =
      let (x', y') = pixelXY x y
          (w', h') = (w/320.0*xw, h/200.0*yh)
      in S.rect ! A.width (toValue w') ! A.height (toValue h') ! ax x' ! ay y'

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
