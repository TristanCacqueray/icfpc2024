-- For HasField instance
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spaceship where

import Data.Attoparsec.Text qualified as P
import GHC.Records (HasField (getField))
import Geomancy.IVec2
import RIO
import RIO.List qualified as List
import RIO.List.Partial qualified as Partial
import RIO.Map qualified as Map
import RIO.Text qualified as Text

import ProgCon.Eval
import ProgCon.Parser
import ProgCon.Printer qualified as Printer

optimizeOrder :: [IVec2] -> [IVec2]
optimizeOrder = go [] (ivec2 0 0)
 where
  -- Order by repeatedly finding the closes target from the previous location 'pos'
  go :: [IVec2] -> IVec2 -> [IVec2] -> [IVec2]
  go acc _pos [] = reverse acc
  go acc pos (x : xs) =
    let
      -- Get the new closest post and the list of remaining targets
      ((_, closestPos), others) = foldl' (go' pos) ((idist pos x, x), []) xs
    in
      go (closestPos : acc) closestPos others

  -- Separate the closest position from the rest
  go' :: IVec2 -> ((Int32, IVec2), [IVec2]) -> IVec2 -> ((Int32, IVec2), [IVec2])
  go' pos (prev@(prevClosestDistance, prevClosestPos), others) target
    | -- The current target is the new closest, move the previous closest to the others list
      targetDist < prevClosestDistance =
        ((targetDist, target), prevClosestPos : others)
    | -- otherwise keep the previous closest
      otherwise =
        (prev, target : others)
   where
    targetDist = idist pos target

idist :: IVec2 -> IVec2 -> Int32
idist p t = dist.x + dist.y
 where
  dist = abs (p - t)

-- | Distance between 2 points
fdist :: IVec2 -> IVec2 -> Float
fdist p q = sqrt $ fromIntegral (q.x - p.x) ** 2 + fromIntegral (q.y - p.y) ** 2

solve₂ :: [IVec2] -> String
solve₂ targets = solveRecursively 3 0 initialShip targets

solveRecursively :: Word -> Int -> Ship -> [IVec2] -> String
solveRecursively _ _ _ [] = ""
solveRecursively lookahead awareness ship (target : targets) =
  let
    course : _ = predictRoute lookahead (target : take awareness targets) ship
    newShip = head (forecast ship [course])
  in
    thrustChar course : solveRecursively lookahead awareness newShip (if target == newShip.pos then targets else target : targets)

solve :: [IVec2] -> String
solve targets = (Partial.minimumBy (compare `on` length) . fmap ($ targets)) [solve₂ . optimizeOrder]

solve₁ :: [IVec2] -> String
solve₁ = go [] initialShip
 where
  go acc _ship [] = concat (reverse acc)
  go acc ship (target : rest) = go (thrusts : acc) newShip rest
   where
    (thrusts, newShip) = genCruise ship target

type Target = IVec2

genCruise :: Ship -> Target -> (String, Ship)
genCruise iship target = go "" iship (fdist iship.pos target)
 where
  go :: String -> Ship -> Float -> (String, Ship)
  go acc ship prevDist
    | ship.pos == target = (reverse newAcc, newShip)
    | newDist > prevDist = error $ "Not getting closer from " <> show ship <> " to " <> show thrust <> " prevDist " <> show prevDist <> " newDist " <> show newDist
    | otherwise = go newAcc newShip newDist
   where
    thrust = genShipThrust ship target
    newShip = applyThrust thrust ship
    newDist = fdist newShip.pos target
    newAcc = thrustChar thrust : acc

-- | Compute the thrust to apply for reaching the target.
genShipThrust :: Ship -> Target -> IVec2
genShipThrust ship target =
  -- traceShow ("genShipThrust" :: String, ship, target) $
  ivec2
    (dbgVal "tx" $ genThrust ship.vel.x ship.pos.x target.x)
    (dbgVal "ty" $ genThrust ship.vel.y ship.pos.y target.y)

-- | A basic one thrust implementation
genThrust :: Int32 -> Int32 -> Int32 -> Int32
genThrust velocity start end
  | start == end =
      if abs velocity > 1 then error "Going too fast!" else negate velocity
  -- \| abs velocity > 0 && start + accelThrust == end = dbgVal "lastCruise!" 0
  | distance < brakingDistance = dbgVal "decel!" $ negate accelThrust
  | distance >= nextBrakingDistance = dbgVal "accel!" $ accelThrust
  | otherwise = dbgVal "cruising!" 0
 where
  distance = dbgVal "dist" $ abs (start - end)
  accelThrust = dbgVal "accelThrust" $ signum (end - start)
  brakingDistance = dbgVal "brakingDistance" $ calcBrakingDistance (abs velocity)
  nextBrakingDistance = dbgVal "nextBrake" $ calcBrakingDistance (abs velocity + 1)

dbgVal -- (Show a) =>
  :: String -> a -> a
dbgVal _name v =
  -- traceShow (_name <> ":" <> show v)
  v

calcBrakingDistance :: Int32 -> Int32
calcBrakingDistance x = x * (x + 1) `div` 2

-- | Update the ship based on a new thrust
applyThrust :: IVec2 -> Ship -> Ship
applyThrust thrust ship = Ship {vel, pos}
 where
  vel = ship.vel + thrust
  pos = ship.pos + vel

data Ship = Ship
  { pos :: IVec2
  , vel :: IVec2
  }
  deriving (Show)

initialShip :: Ship
initialShip = Ship (ivec2 0 0) (ivec2 0 0)

-- | This returns the list of missed target, it should be empty for a successfull solution
validateSolution :: [IVec2] -> String -> [IVec2]
validateSolution wanted = go wanted initialShip
 where
  go acc _ship [] = acc
  go acc ship (thrust : rest) =
    let newShip = applyThrustChar thrust ship
        newAcc = filter (/= newShip.pos) acc
    in  go newAcc newShip rest

-- | Update ship based on solution char
applyThrustChar :: Char -> Ship -> Ship
applyThrustChar c Ship {pos, vel} = Ship {pos = newPos, vel = newVel}
 where
  newVel = vel + charVel c
  newPos = pos + newVel

charVel :: Char -> IVec2
charVel = \case
  '9' -> ivec2 1 1
  '8' -> ivec2 0 1
  '7' -> ivec2 (-1) 1
  '6' -> ivec2 1 0
  '5' -> ivec2 0 0
  '4' -> ivec2 (-1) 0
  '3' -> ivec2 1 (-1)
  '2' -> ivec2 0 (-1)
  '1' -> ivec2 (-1) (-1)
  _ -> ivec2 0 0

thrustChar :: IVec2 -> Char
thrustChar thrust =
  case (thrust.x, thrust.y) of
    (1, 1) -> '9'
    (0, 1) -> '8'
    (-1, 1) -> '7'
    (1, 0) -> '6'
    (0, 0) -> '5'
    (-1, 0) -> '4'
    (1, -1) -> '3'
    (0, -1) -> '2'
    (-1, -1) -> '1'
    _ -> 'X'

testPath :: String -> [IVec2]
testPath = go [initialShip.pos] initialShip
 where
  go acc _ship [] = reverse acc
  go acc ship (c : rest) =
    let newShip = applyThrustChar c ship
    in  go (newShip.pos : acc) newShip rest

spec :: IO ()
spec = do
  cmp resPath expPath
  forM_ cruises \(res, expected) -> cmp res expected
  checkSolve [(-3, 0)] "4556"
  checkSolve [(-3, 0), (-3, 2)] "4556852"
 where
  cmp res expected = do
    when (res /= expected) $
      error $
        "oops: " <> show res <> ", expected: " <> show expected
  checkSolve xs = cmp (solve $ map (uncurry ivec2) xs)
  cruises =
    [ (testCruise 1 1, "91")
    , (testCruise 0 0, "5")
    , (testCruise 5 0, "66454")
    , (testCruise 6 0, "66544")
    , (testCruise 7 0, "665454")
    , (testCruise 5 3, "96424")
    , (testCruise 2 2, "951")
    , (testCruise (-2) (-2), "159")
    ]
  testCruise x y = fst $ genCruise initialShip (ivec2 x y)
  expPath = map (uncurry ivec2) [(0, 0), (0, -1), (1, -3), (3, -5), (6, -7), (9, -9), (13, -10)]
  resPath = testPath "236659"

parseInput :: Text -> [IVec2]
parseInput txt = either (error . mappend "oops: ") id $ P.parseOnly (P.many1' coordP <* P.endOfInput) txt

coordP :: P.Parser IVec2
coordP = do
  i1 <- P.signed P.decimal
  P.skipSpace
  i2 <- P.signed P.decimal
  P.skipSpace
  pure (ivec2 i1 i2)

directionVectors :: [IVec2]
directionVectors = fmap charVel "123456789"

chebyshev :: (Num number) => IVec2 -> IVec2 -> number
chebyshev v₁ v₂ = fromIntegral do max (abs (v₂.x - v₁.x)) (abs (v₂.y - v₁.y))

-- | Given a travel plan, tell where we shall be over time.
forecast :: Ship -> [IVec2] -> [Ship]
forecast Ship {..} travelPlan =
  let velocities = scanl (+) vel travelPlan
      positions = scanl (+) pos (tail velocities)
  in  tail (zipWith (\pos vel -> Ship {..}) positions velocities)

-- | Evaluate the minimal distance to each of the given targets over given
-- number of steps given all possible travel plans, pick the best.
predictRoute :: Word -> [IVec2] -> Ship -> [IVec2]
predictRoute horizon targets ship = head (List.sortOn (cost ship targets) (travelPlans horizon))

cost :: Ship -> [IVec2] -> [IVec2] -> Float
cost ship targets = minimum . fmap (weightedSumOfDistances targets) . forecast ship

travelPlans :: Word -> [[IVec2]]
travelPlans horizon = iterate ((=<<) \list -> fmap (: list) directionVectors) [[]] Partial.!! fromIntegral horizon

weightedSumOfDistances :: [IVec2] -> Ship -> Float
weightedSumOfDistances targets ship = sum (zipWith (*) (weights (List.genericLength targets)) (fmap (chebyshev ship.pos) targets))

weights :: Word -> [Float]
weights number = fmap (((1 / 2) **) . fromIntegral) [1 .. number]

solveExpression :: Natural -> Expr -> Either String Expr
solveExpression nr inputExpression = do
  evaluatedInputExpression <- evalExpr inputExpression
  inputText <- case evaluatedInputExpression of
    EStr text -> Right text
    somethingElse -> Left (show somethingElse)
  let parsedInput = parseInput inputText
  let solution = solve parsedInput
  let solutionExpression = EStr $ Text.pack $ unwords ["solve", "spaceship" <> show nr, solution]
  pure solutionExpression

validateExpression :: Expr -> Either String Float
validateExpression inputExpression = do
  evaluatedInputExpression <- evalExpr inputExpression
  inputText <- case evaluatedInputExpression of
    EStr text -> Right text
    somethingElse -> Left (show somethingElse)
  let parsedInput = parseInput inputText
  let solution = solve parsedInput
  case validateSolution parsedInput solution of
    [] -> Right do 100 / (fromIntegral . Text.length . Printer.print . EStr . Text.pack) solution
    list -> Left (show list)

-- | Helpers to access ivec2 components with '.x' and '.y'
instance HasField "x" IVec2 Int32 where getField v = withIVec2 v \x _ -> x

instance HasField "y" IVec2 Int32 where getField v = withIVec2 v \_ y -> y
