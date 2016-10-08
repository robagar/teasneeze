module Util where

require :: Either String a -> a
require (Left  e) = error e
require (Right a) = a

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

tanDeg :: Floating a => a -> a
tanDeg = tan.deg2rad

atanDeg :: Floating a => a -> a
atanDeg = rad2deg.atan

rad2deg :: Floating a => a -> a 
rad2deg = (180*).(/pi)

deg2rad :: Floating a => a -> a 
deg2rad =  (pi*).(/180)
