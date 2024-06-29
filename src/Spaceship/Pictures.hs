module Spaceship.Pictures where

import Geomancy.IVec2
import Graphics.Gloss

drawSpaceship :: IVec2 -> [IVec2] -> Picture
drawSpaceship pos targets = Pictures $ drawPos green pos : map (drawPos orange) targets
  where
    drawPos col xy = withIVec2 xy \x y -> do
      Translate (fromIntegral x) (fromIntegral y) $ Color col $ circleSolid 1

posPoint :: IVec2 -> (Float, Float)
posPoint xy = withIVec2 xy \x y -> (fromIntegral x, fromIntegral y)
