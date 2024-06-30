module Spaceship.Pictures where

import Geomancy.IVec2
import Graphics.Gloss
import Spaceship

drawSpaceship :: IVec2 -> [Target] -> Picture
drawSpaceship pos targets = Pictures (Line path : drawDot green pos : map (drawDot black) targets)
 where
  path = map mkPoint (pos:targets)
  mkPoint p = (fromIntegral p.x, fromIntegral p.y)
  drawDot col coord =
    Translate (fromIntegral coord.x) (fromIntegral coord.y) $ Color col $ circleSolid 0.2
