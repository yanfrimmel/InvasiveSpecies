module Utils where

import           Foreign.C.Types
import           Reflex.SDL2

fromPointCIntToVectorCFloat :: Point V2 CInt -> V2 CFloat
fromPointCIntToVectorCFloat (P (V2 x y)) = V2 (fromIntegral x) (fromIntegral y)

fromPointCIntToPointCFloat :: Point V2 CInt -> Point V2 CFloat
fromPointCIntToPointCFloat (P (V2 x y)) = P $ V2 (fromIntegral x) (fromIntegral y)

fromPointToVector :: Point V2 a -> V2 a
fromPointToVector (P (V2 x y)) = V2 x y

fromPointCFloatToPointCInt :: Point V2 CFloat -> Point V2 CInt
fromPointCFloatToPointCInt (P (V2 x y)) = P $ V2 (round x) (round y)
