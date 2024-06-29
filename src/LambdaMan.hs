{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan where

import RIO hiding (map)
import RIO.List qualified as List
import Prelude qualified

import LambdaMan.Pictures
import ProgCon.Parser

import Data.Array
import Data.Array.MArray
import Data.Array.ST.Safe
import Graphics.Gloss qualified as Gloss
import Linear
import RIO.List.Partial qualified as Partial

solveExpression :: Expr -> Either String Expr
solveExpression = (const . Right) (EStr "LLLDURRRUDRRURR")

validateExpression :: Expr -> Either String Float
validateExpression = const (Left "Not implemented.")

readMap :: FilePath -> IO (Array (V2 Int) Int)
readMap = fmap makeMapArray . Prelude.readFile

makeMapArray :: String -> Array (V2 Int) Int
makeMapArray string = runSTArray do
  map <- newArray (V2 0 0, V2 (width - 1) (height - 1)) 0
  forM_ (rows `zip` [0 ..]) \(row, j) ->
    forM_ (row `zip` [0 ..]) \(character, i) ->
      writeArray map (V2 i j) (meaning character)
  pure map
 where
  rows = lines string
  height = List.genericLength rows
  width = Partial.maximum (0 :| fmap List.genericLength rows)

meaning :: Char -> Int
meaning = \case
  '.' -> 0
  '#' -> 1
  'L' -> 2
  character -> error do "Meaningless character " <> show character <> "!"

drawMap :: Natural -> IO ()
drawMap level = do
  map <- readMap ("examples/lambdaman/" <> show level <> "/problem.string")
  Gloss.display Gloss.FullScreen backgroundColour (pictureOfMap map)
