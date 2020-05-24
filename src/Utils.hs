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

xFromPoint :: (Num a ) => Point V2 a -> a
xFromPoint (P v) = xFromV2 v

yFromPoint :: (Num a ) => Point V2 a -> a
yFromPoint (P v) = yFromV2 v

xFromV2 :: (Num a ) => V2 a -> a
xFromV2 (V2 x _) = x

yFromV2 :: (Num a ) => V2 a -> a
yFromV2 (V2 _ y) = y

composeSame :: (a -> b -> a) -> (a -> b -> a) -> a -> b -> a
composeSame g f x y = g (f x y) y
