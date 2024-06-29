{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan where

import RIO hiding (Map, map)
import RIO.List qualified as List
import Prelude qualified

import LambdaMan.Pictures
import LambdaMan.Types
import ProgCon.Parser

import Data.Array hiding (array, index)
import Data.Array.MArray hiding (index)
import Data.Array.ST.Safe hiding (index)
import Graphics.Gloss qualified as Gloss
import Graphics.Gloss.Interface.Pure.Simulate qualified as Gloss
import Linear
import RIO.List.Partial qualified as Partial

solveExpression :: Expr -> Either String Expr
solveExpression = (const . Right) (EStr "LLLDURRRUDRRURR")

validateExpression :: Expr -> Either String Float
validateExpression = const (Left "Not implemented.")

readMap :: FilePath -> IO Map
readMap path = do
  array <- (fmap makeMapArray . Prelude.readFile) path
  let me = (fst . fromMaybe (error "There should be a Lambda Man somewhere!") . List.find (\(_, spot) -> spot == Me) . assocs) array
  pure Map {..}

makeMapArray :: String -> Array (V2 Int) Spot
makeMapArray string = runSTArray do
  map <- newArray (V2 0 0, V2 (width - 1) (height - 1)) Floor
  forM_ (rows `zip` [0 ..]) \(row, j) ->
    forM_ (row `zip` [0 ..]) \(character, i) ->
      writeArray map (V2 i j) (meaning character)
  pure map
 where
  rows = lines string
  height = List.genericLength rows
  width = Partial.maximum (0 :| fmap List.genericLength rows)

meaning :: Char -> Spot
meaning = \case
  '.' -> Floor
  '#' -> Wall
  'L' -> Me
  character -> error do "Meaningless character " <> show character <> "!"

drawMap :: Natural -> IO ()
drawMap level = do
  map <- readMap ("examples/lambdaman/" <> show level <> "/problem.string")
  Gloss.simulate Gloss.FullScreen backgroundColour 4 (markLegalSteps map) pictureOfMap animateMap

animateMap :: Gloss.ViewPort -> Float -> Map -> Map
animateMap _ _ map = case legalSteps map of
  [] -> map
  (direction : _) -> (markLegalSteps . fromMaybe (error "Legal step failed!") . flip makeStep direction . unmarkLegalSteps) map

vectorOfDirection :: Direction -> V2 Int
vectorOfDirection U = V2 0 (-1)
vectorOfDirection R = V2 1 0
vectorOfDirection D = V2 0 1
vectorOfDirection L = V2 (-1) 0

(?) :: (Ix index) => Array index value -> index -> Maybe value
array ? index
  | inRange (bounds array) index = Just do array ! index
  | otherwise = Nothing
infixl 9 ?

canStepOnThis :: Spot -> Bool
canStepOnThis = \case
  Floor -> True
  LegalStep -> True
  _ -> False

makeStep :: Map -> Direction -> Maybe Map
makeStep Map {..} direction =
  let newMe = (me + vectorOfDirection direction)
  in  case array ? newMe of
        Just tile
          | canStepOnThis tile ->
              Just
                Map
                  { me = newMe
                  , array = runSTArray do
                      mutableArray <- thaw array
                      writeArray mutableArray me FootStep
                      writeArray mutableArray newMe Me
                      pure mutableArray
                  }
        _ -> Nothing

legalSteps :: Map -> [Direction]
legalSteps map = filter (isJust . makeStep map) [minBound .. maxBound]

markLegalSteps :: Map -> Map
markLegalSteps map@Map {..} =
  map
    { array = runSTArray do
        mutableArray <- thaw array
        forM_ (legalSteps map) \direction -> writeArray mutableArray (me + vectorOfDirection direction) LegalStep
        pure mutableArray
    }

unmarkLegalSteps :: Map -> Map
unmarkLegalSteps map@Map {..} =
  map
    { array = runSTArray do
        mutableArray <- thaw array
        forM_ (legalSteps map) \direction -> writeArray mutableArray (me + vectorOfDirection direction) Floor
        pure mutableArray
    }
