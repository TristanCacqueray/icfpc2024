{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan.Pictures where

import RIO hiding (fold)
import RIO.ByteString qualified as ByteArray
import RIO.Map qualified as Map

import LambdaMan.Types

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Array hiding (array)
import Data.Functor.Foldable
import Graphics.Gloss
import Linear

bigness :: Float
bigness = 20

area :: V2 Float -> Picture
area (V2 width height) = Translate (width / 2) (height / 2) do
  Pictures
    [ Color (greyN 0.1) do rectangleSolid (width + 1.5) (height + 1.5)
    , Color (greyN 0.2) do rectangleWire (width + 1.5) (height + 1.5)
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

pictureOfBoard :: Board -> Picture
pictureOfBoard Board {..} = do
  Pictures
    [ area size
    , (Pictures . fmap drawTile . assocs) array
    ]
 where
  size = (fmap fromIntegral . snd . bounds) array
drawTile :: (V2 Int, Spot) -> Picture
drawTile (fmap fromIntegral -> V2 x y, entity) = case entity of
  Floor -> Blank
  Wall -> wall x y
  Me -> hero x y
  FootStep -> footstep x y
  LegalStep -> legalStep x y

pictureOfAnimationState :: AnimationState -> Picture
pictureOfAnimationState AnimationState {..} =
  (Scale bigness bigness . Translate (-width / 2) (-height / 2) . Pictures)
    [pictureOfBoard board, pictureOfSolution board.me (ByteArray.unpack solution {- , pictureOfSpanningTree spanningTree -})]
 where
  V2 width height = (fmap fromIntegral . snd . bounds) board.array

pictureOfSolution :: V2 Int -> [Word8] -> Picture
pictureOfSolution =
  fmap Pictures . fix \recurse me -> \case
    [] -> []
    (x : xs) ->
      let
        direction = directionOfWord8 x
        newMe = me + vectorOfDirection direction
      in
        smallArrow red (fmap fromIntegral me) direction : recurse newMe xs

smallArrow :: Color -> V2 Float -> Direction -> Picture
smallArrow colour (V2 x y) direction =
  Translate x y do
    Rotate ((fromIntegral . fromEnum) direction * (-90)) do
      Color (withAlpha 0.33 colour) do
        Polygon [(0.2, 0.4), (0, -0.4), (-0.2, 0.4)]

pictureOfSpanningTree :: Cofree (Map Direction) (V2 Int) -> Picture
pictureOfSpanningTree = Pictures . fold folding
 where
  folding (me :< leaves) = ((=<<) (uncurry drawArrow) . Map.toList) leaves
   where
    locationOfArrow direction = fmap fromIntegral me + (fmap fromIntegral . vectorOfDirection) direction / 2
    drawArrow direction otherPictures =
      smallArrow cyan (locationOfArrow direction) direction
        : otherPictures
