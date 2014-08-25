module Main where

import System.Environment
import Graphics.Gloss.Raster.Field
import Data.Maybe

spheres = [(Sphere (0.5, 0.2, -4.0) 0.01),
		   (Sphere (-0.5, 0.2, -2.0) 0.01),
		   (Sphere (0.5, -0.3, -3.0) 0.01)]

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
	let 
		ray = pixelRay p
	in if intersectSphere ray (head spheres) then rawColor 1.0 1.0 1.0 1.0 
		else if intersectSphere ray (last spheres) then rawColor 1.0 0.0 0.0 1.0
		else if intersectSphere ray (head (tail spheres)) then rawColor 0.0 1.0 1.0 1.0
		else rawColor 0.0 0.0 0.0 1.0

intersectSphere :: Ray -> Sphere -> Intersection
intersectSphere (Ray origin dir) (Sphere centre radius) =
	let 
		radius2 = radius * radius
		l = subV origin centre
		a = dotV dir dir
		b = 2 * dotV dir l
		c = (dotV l l) - 1
		(t0, t1) = solveQuadratic a b c
	in isJust t0

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
dotV :: Vec3 -> Vec3 -> Float
dotV (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

type Vec3 = (Float, Float, Float)
type Point3 = Vec3
type Intersection = Bool
data Sphere = Sphere Point3 Float
data Ray = Ray Point3 Vec3

solveQuadratic :: Float -> Float -> Float -> (Maybe Float, Maybe Float)
solveQuadratic a b c
	| discr < 0   = (Nothing, Nothing)
	| discr == 0  = (Just (-0.5 * b / a), Nothing)
	| discr > 0   = solveQuadratic' a b c discr
	where discr = b * b - 4 * a * c

solveQuadratic' :: Float -> Float -> Float -> Float -> (Maybe Float, Maybe Float)
solveQuadratic' a b c discr = 
	let sign = if (b > 0) then 1 else -1
	    q = -0.5 * (b + sign * sqrt discr)
	    x0 = q / a
	    x1 = c / q
	in 
		if (x0 > x1) then (Just x1, Just x0) else (Just x0, Just x1) 