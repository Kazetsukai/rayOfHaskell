module Main where

import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Raster.Field

main :: IO ()
main = 
    animateField
        (InWindow "Test" (600, 400) (10, 10))
        (1,1)
        (\t (x,y) -> rawColor x 0.3 t 1.0)