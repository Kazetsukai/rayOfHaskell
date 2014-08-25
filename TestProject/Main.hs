module Main where

import System.Environment
import Graphics.Gloss.Raster.Field
import Data.Vector.Unboxed

width = 1024
height = 1024

aspectRatio = (fromIntegral width :: Float) / (fromIntegral height :: Float)

main :: IO ()
main = do
    animateField
        (InWindow "Test" (width, height) (10, 10))
        (1,1)
        pixelColor

pixelColor :: Float -> Point -> Color
pixelColor t p = 
	let Ray point vec = pixelRay p
	    (x, y, z)     = vec
	in rawColor x y z 1.0

pixelRay :: Point -> Ray
pixelRay (x, y) = Ray (0, 0, 0) (normalise (x, y, -1))

normalise :: Vec3 -> Vec3
normalise (x, y, z) = 
	let len = lengthV (x, y, z)
	in (x / len, y / len, z / len)

lengthV :: Vec3 -> Float
lengthV (x, y, z) = sqrt (x*x + y*y + z*z)

addV :: Vec3 -> Vec3 -> Vec3
addV (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
subV :: Vec3 -> Vec3 -> Vec3
subV (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
multV :: Vec3 -> Float -> Vec3
multV (x, y, z) a = (x*a, y*a, z*a)

type Vec3 = (Float, Float, Float)
type Point3 = Vec3
data Ray = Ray Point3 Vec3