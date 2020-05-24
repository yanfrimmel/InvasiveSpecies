{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Input where

import qualified Data.Label      as L
import           Data.Maybe
-- import qualified Debug.Trace     as D
import           Foreign.C.Types
import           Reflex.SDL2
import           System.Random
import           Types
import           Utils
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

----------- ai ----------------------------
updateGameObjects :: CFloat -> GameState -> GameState
updateGameObjects delta s =
  if null objs
    then s
  else
    GameState {_camera = _camera s, _player = _player s, _gameObjects = newObjs, _randomGene = g}
    where
          objs = L.get gameObjects s
          newObjs = filter (filterOutDeadObjects) (map (updateGameObject g delta objs) objs)
          g = snd $ next $ _randomGene s

filterOutDeadObjects :: GameObject -> Bool
filterOutDeadObjects o =
  case _gameObjectType o of
    Player -> True
    AnimalTag a ->
       L.get hp a > 0
    _ ->  True

updateGameObject :: StdGen -> CFloat -> [GameObject] -> GameObject -> GameObject
updateGameObject g delta others me =
  case _gameObjectType me of
    Player -> me
    AnimalTag a ->
      if L.get hp a <= 0
        then me
      else updateAnimalState g delta others me
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

moveToDestination :: CFloat -> Point V2 CFloat -> GameObject -> GameObject
moveToDestination delta target o =
  if distanceBetweenPoints > proximityThreshold
    then L.set position (P newPosition) o
  else L.set destination Nothing o
  where
    s = _speed o
    currPos = _position o
    direction = normalize $ fromPointToVector target ^-^ fromPointToVector currPos
    newPosition = fromPointToVector currPos ^+^ (direction ^* (s * delta))
    distanceBetweenPoints = distance (fromPointToVector currPos) (fromPointToVector target)
    proximityThreshold = fromIntegral (xFromV2 $ _sizeT $ _texture o) / 8

updateAnimalState :: StdGen -> CFloat -> [GameObject] -> GameObject -> GameObject
updateAnimalState g delta others o =
  L.set gameObjectType (AnimalTag updatedAnimal) updatedDestination
  where (GameObject _ _ _ _ _ (AnimalTag a)) = o
        updatedAnimal = (updateHydration `composeSame` updateNutrition) a delta
        updatedDestination = if isJust $ _destination o
                               then moveToDestination delta (fromJust $ currentDestination) o
                             else createRandomDestination g o
        currentDestination = _destination o

createRandomDestination :: StdGen -> GameObject -> GameObject
createRandomDestination g o = L.set destination (Just (P (V2 (fst rx) (fst ry)))) o
  where rx = randomR (4500 :: CFloat, 5500 :: CFloat) g -- TODO: implement
        ry = randomR (4500 :: CFloat, 5500 :: CFloat) (snd rx)
