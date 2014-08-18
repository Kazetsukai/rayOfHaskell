module Main where

import Data.Maybe()
import Control.Concurrent()
import Control.Monad
import System.Exit
import Data.Time.Clock.POSIX
import Foreign.Storable
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr
import Foreign

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Surface
import Graphics.UI.SDL.Types

main :: IO ()
main =
  SDL.withInit [InitEverything] $
  withWindow "Hello World!" (Position 100 100) (Size 640 480) [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \ren -> do
    tex <- createTextureFromSurface ren =<< loadBMP "test.bmp"
    lockTexture tex Nothing (\p -> return 20)
    unlockTexture tex

    forever $ do
    	ev <- pollEvent
    	if fmap eventData ev == Just Quit
    		then exitSuccess
    		else return ()

    	time <- getPOSIXTime
    	renderClear ren
    	renderCopy ren tex Nothing (Just (Rect ((round (time * 300)) `mod` 600) 0 200 200))
    	renderPresent ren


getPixel32 :: Int -> Int -> Surface -> IO Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    Pixel `liftM` peekElemOff pixels ((y * surfaceGetWidth s) + x)

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) pixel


surfaceGetPixels :: Surface -> IO Pixels
surfaceGetPixels surface
    = withForeignPtr surface $
      peek SDL_Surface, pixels