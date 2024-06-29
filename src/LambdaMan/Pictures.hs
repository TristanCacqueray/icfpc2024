{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan.Pictures where

import RIO hiding (map)

import Data.Array
import Graphics.Gloss
import Linear

bigness :: Float
bigness = 20

area :: V2 Float -> Picture
area (V2 width height) = Translate (width / 2) (height / 2) do
  Pictures
    [ Color (greyN 0.1) do rectangleSolid (width + 1) (height + 1)
    , Color (greyN 0.2) do rectangleWire (width + 1) (height + 1)
    ]
wall :: Float -> Float -> Picture
wall x y = Translate x y let z = 0.4 in Color (greyN 0.6) do Polygon [(z, z), (z, -z), (-z, -z), (-z, z)]
hero :: Float -> Float -> Picture
hero x y = Translate x y do Color green do ThickCircle 0.15 0.3
footstep :: Picture
footstep = Color (greyN 0.3) do ThickCircle 0.2 0.2
backgroundColour :: Color
backgroundColour = greyN 0

pictureOfMap :: Array (V2 Int) Int -> Picture
pictureOfMap map = (Scale bigness bigness . Translate (-width / 2) (-height / 2)) do
  Pictures
    [ area size
    , (Pictures . fmap drawTile . assocs) map
    ]
 where
  size@(V2 width height) = (fmap fromIntegral . snd . bounds) map
drawTile :: (V2 Int, Int) -> Picture
drawTile (fmap fromIntegral -> V2 x y, entity) = case entity of
  0 -> Blank
  1 -> wall x y
  2 -> hero x y
  _ -> Blank
