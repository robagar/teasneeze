module Spherical where

type SphericalCoords = (Float, Float, Float)

cartesianToSpherical :: Floating a => a -> a -> a -> (a, a, a)
cartesianToSpherical = undefined

sphericalToCartesian :: Floating a => a -> a -> a -> (a, a, a)
sphericalToCartesian r phi theta = (x, y, z)
    where x = r * (cos theta) * (cos phi)
          y = r * (sin theta)
          z = r * (cos theta) * (sin phi)

