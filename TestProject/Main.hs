module Main where

import System.Environment
import Graphics.Gloss.Raster.Field
import Data.Maybe

spheres = [(Sphere (0.5, 0.2, -2.0) 0.5),
		   (Sphere (-0.5, 0.2, -2.0) 0.8),
		   (Sphere (0.5, -0.3, -2.7) 1.0),
		   (Sphere (0.5, -12.3, -2.7) 10.0)]

lights = [PointLight (0.0, 1.8, -0.5),
		  PointLight (2.0, 1.0, 0.0)]

width = 1024
height = 1024

aspectRatio = (fromIntegral width :: Float) / (fromIntegral height :: Float)

main :: IO ()
main = do
    animateField
        (InWindow "Test" (width, height) (10, 10))
        (5,5)
        pixelColor

pixelColor :: Float -> Point -> Color
pixelColor t p = case mx of
		Intersection ray object d p ->
			sum (map (lightColour p objects) lights)
		_ -> 
			rawColor 0.0 0.0 0.0 1.0

	where 
		objects = map (throbSphere t) spheres
		mx = rayTrace (pixelRay p) objects

lightColour :: Point3 -> [Object] -> Light -> Color
lightColour p objects (PointLight l) = 
	if (canSee p l objects) 
	then rawColor lum lum lum 1.0
	else rawColor 0.0 0.0 0.0 1.0
	where lum = 1.0 / dotV (subV p l) (subV p l)
lightColour _ _ _ = rawColor 0.0 0.0 0.0 1.0

rayTrace :: Ray -> [Object] -> Intersection
rayTrace r objects = 
	minimum $ map (intersect r) objects

canSee :: Point3 -> Point3 -> [Object] -> Bool
canSee p1 p2 objects =
	case i of
		NoIntersect -> True
		_ -> False
	where i = rayTrace (Ray p1 (subV p2 p1)) objects
	

intersect :: Ray -> Object -> Intersection
intersect (Ray origin dir) (Sphere centre radius) =
	let 
		radius2 = radius * radius
		l = subV origin centre
		a = dotV dir dir
		b = 2 * dotV dir l
		c = (dotV l l) - radius2
		(t0, t1) = solveQuadratic a b c
	in if (isJust t0) && ((fromJust t0) > -0.00001)
		then Intersection (Ray origin dir) (Sphere centre radius) (fromJust t0) (addV (multV dir (fromJust t0)) origin)
		else NoIntersect

throbSphere :: Float -> Object -> Object
throbSphere a (Sphere centre radius) = Sphere centre (radius + (sin a) / 10)
throbSphere a b = b

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
data Intersection = Intersection Ray Object Float Point3 | NoIntersect deriving(Eq)
data Object = Sphere Point3 Float deriving(Eq)
data Ray = Ray Point3 Vec3 deriving(Eq)
data Light = PointLight Point3 deriving(Eq)

instance Ord Intersection where
	compare (Intersection _ _ dist1 _) (Intersection _ _ dist2 _) = compare dist1 dist2
	compare a NoIntersect = LT
	compare NoIntersect a = GT

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