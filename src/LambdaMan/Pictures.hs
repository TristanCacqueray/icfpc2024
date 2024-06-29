{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan.Pictures where

import RIO hiding (Map, map)

import LambdaMan.Types

import Data.Array hiding (array)
import Graphics.Gloss
import Linear

bigness :: Float
bigness = 20

area :: V2 Float -> Picture
area (V2 width height) = Translate ((width + 1) / 2) ((height + 1) / 2) do
  Pictures
    [ Color (greyN 0.1) do rectangleSolid (width + 1) (height + 1)
    , Color (greyN 0.2) do rectangleWire (width + 1) (height + 1)
    ]
wall :: Float -> Float -> Picture
wall x y = Translate x y let z = 0.4 in Color (greyN 0.6) do Polygon [(z, z), (z, -z), (-z, -z), (-z, z)]
hero :: Float -> Float -> Picture
hero x y = Translate x y do Color green do ThickCircle 0.15 0.3
footstep :: Float -> Float -> Picture
footstep x y = Translate x y do Color (withAlpha 0.5 orange) do ThickCircle 0.1 0.2
legalStep :: Float -> Float -> Picture
legalStep x y = Translate x y let z = 0.2 in Color (withAlpha 0.5 green) do Polygon [(z, z), (z, -z), (-z, -z), (-z, z)]
backgroundColour :: Color
backgroundColour = greyN 0

pictureOfMap :: Map -> Picture
pictureOfMap Map {..} = (Scale bigness bigness . Translate (-width / 2) (-height / 2)) do
  Pictures
    [ area size
    , (Pictures . fmap drawTile . assocs) array
    ]
 where
  size@(V2 width height) = (fmap fromIntegral . snd . bounds) array
drawTile :: (V2 Int, Spot) -> Picture
drawTile (fmap fromIntegral -> V2 x y, entity) = case entity of
  Floor -> Blank
  Wall -> wall x y
  Me -> hero x y
  FootStep -> footstep x y
  LegalStep -> legalStep x y
