{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Input where

import qualified Data.Label      as L
import           Foreign.C.Types
import           Reflex.SDL2
import           System.Random
import           Types

----------- mouse input -------------
getMouseButtonClickPosition :: MouseButtonEventData -> Point V2 CInt
getMouseButtonClickPosition dat = pos
  where P pos32 = mouseButtonEventPos dat
        pos = P (fromIntegral <$> pos32)

getMousePosition :: MouseMotionEventData -> Point V2 CInt
getMousePosition dat = pos
  where P pos32 = mouseMotionEventPos dat
        pos = P (fromIntegral <$> pos32)

isLeftButtonDown :: MouseButtonEventData -> Bool
isLeftButtonDown m =
    mouseButtonEventMotion m == Pressed && mouseButtonEventButton m == ButtonLeft

isLeftButtonUp :: MouseButtonEventData -> Bool
isLeftButtonUp m =
    mouseButtonEventMotion m == Released && mouseButtonEventButton m == ButtonLeft

----------- ai input ----------------------------
updateGameObjects :: StdGen -> CFloat -> GameState -> GameState
updateGameObjects g delta s =
  if null objs
    then s
  else
    GameState {_camera = _camera s, _player = _player s, _gameObjects = newObjs}
    where
          objs = L.get gameObjects s
          newObjs = filter (filterOutDeadObjects) (map (updateGameObject g delta objs) objs)

filterOutDeadObjects :: GameObject -> Bool
filterOutDeadObjects o =
  case _gameObjectType o of
    Player -> True
    AnimalTag a ->
       L.get hp a > 0
    _ ->  True

updateGameObject :: StdGen -> CFloat -> [GameObject] -> GameObject -> GameObject
updateGameObject g delta theRest me =
  case _gameObjectType me of
    Player -> me
    AnimalTag a ->
      if L.get hp a <= 0
        then me
      else L.set gameObjectType (AnimalTag result) move
      where result = updateNutrition (updateHydration a delta) delta  -- TODO: implement
            move = L.modify position (^+^ P(V2 0.01 0.01)) me -- TODO: implement
            r1 = randomR (1 :: CFloat, 4 :: CFloat) g -- TODO: implement
    _ ->  me

updateHydration :: Animal -> CFloat -> Animal
updateHydration a delta = 
  if L.get hydration a <= 0
    then do
      L.modify hp (subtract delta) a
  else
    L.modify hydration (subtract delta) a 

updateNutrition ::  Animal -> CFloat -> Animal
updateNutrition a delta =
  if L.get nutrition a <= 0
    then L.modify hp (subtract delta) a
  else
    L.modify nutrition (subtract delta) a
