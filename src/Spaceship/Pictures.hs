module Spaceship.Pictures where

import Geomancy.IVec2
import Graphics.Gloss
import Spaceship

drawSpaceship :: IVec2 -> [Target] -> Picture
drawSpaceship pos targets = Pictures $ drawDot green pos : map (drawDot orange) targets
 where
  drawDot col coord =
    Translate (fromIntegral coord.x) (fromIntegral coord.y) $ Color col $ circleSolid 1
