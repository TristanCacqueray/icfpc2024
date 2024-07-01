module Spaceship.Pictures where

import Graphics.Gloss
import Spaceship

data Model = Model
  { ship :: Ship
  , targets :: [Target]
  , thrust :: String
  , reset :: (String, [Target])
  }

newModel :: (String, [Target]) -> Model
newModel reset = Model initialShip (snd reset) (fst reset) reset

drawModel :: Model -> Picture
drawModel model = Pictures (Line path : drawDot green 1.2 model.ship.pos : map (drawDot black 0.8) model.targets)
 where
  path = map mkPoint (model.ship.pos : model.targets)
  mkPoint p = (fromIntegral p.x, fromIntegral p.y)
  drawDot col size coord =
    Translate (fromIntegral coord.x) (fromIntegral coord.y) $ Scale size size $ Color col $ circleSolid 0.2

stepModel :: Float -> Model -> Model
stepModel _ model = case model.thrust of
  [] -> newModel model.reset
  (thrust : thrusts) ->
    let newShip = applyThrustChar thrust model.ship
        newTargets = case model.targets of
          [] -> []
          (target : targets)
            | target == newShip.pos -> targets
            | otherwise -> model.targets
    in  Model newShip newTargets thrusts model.reset
