module Spaceship where

import Data.Attoparsec.Text qualified as P
import Geomancy.IVec2
import RIO

solve :: [IVec2] -> String
solve = go [] initialShip
 where
  go acc _ship [] = concat (reverse acc)
  go acc ship (target : rest) = go (thrusts : acc) newShip rest
   where
    (thrusts, newShip) = cruise ship target

cruise :: Ship -> IVec2 -> (String, Ship)
cruise iship target = go "" iship
 where
  go :: String -> Ship -> (String, Ship)
  go acc ship
    | ship.pos == target =
        -- We arrived at destination, remove acceleration and stop
        (reverse $ thrustChar (decelerate ship) : acc, Ship target (ivec2 0 0))
    | otherwise =
        -- We are on our way, add thrust if necessary and keep on going
        let thrust = accelerate ship target
        in  go (thrustChar thrust : acc) (applyThrust thrust ship)

-- | Update the ship based on a new thrust
applyThrust :: IVec2 -> Ship -> Ship
applyThrust thrust ship = Ship {vel, pos}
 where
  vel = ship.vel + thrust
  pos = ship.pos + vel

-- | Compute the signed thrust to go toward the target
accelerate :: Ship -> IVec2 -> IVec2
accelerate ship target =
  let thrust = signum $ target - ship.pos
  in  thrust - ship.vel

decelerate :: Ship -> IVec2
decelerate ship = negate ship.vel

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
  withIVec2 thrust \x y ->
    case (x, y) of
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
    , (testCruise 2 2, "951")
    , (testCruise (-2) (-2), "159")
    ]
  testCruise x y = fst $ cruise initialShip (ivec2 x y)
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
