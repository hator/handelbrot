module Main where

import Mandelbrot
import Data.Complex (Complex((:+)))
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Event (quitRequested)
import Graphics.UI.SDL.Types (Surface(surfacePixels))
import Foreign.C.String (newCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr
import Foreign.Storable
import Data.Int (Int32(..), Int64(..))
import Data.Word
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import System.Random
import System.IO.Unsafe


sdlWindowPosCentered :: CInt
sdlWindowPosCentered = 0x2fff0000

iterations :: Int
iterations = 100

width :: Int
width = 1024

height :: Int
height = 786

widthF :: Double
widthF = fromIntegral width

heightF :: Double
heightF = fromIntegral height

mandelbrotI :: RealFloat a => Complex a -> Int
mandelbrotI = mandelbrot iterations
{-# INLINE mandelbrotI #-}

while :: Monad m => m Bool -> m a -> m ()
while test action = do
    val <- test
    if val
    then action >> while test action
    else return ()

main :: IO ()
main = do
    videoInit nullPtr
    title <- (newCString "Handelbrot")

    palette <- array (1, iterations) <$> sequence ( [sequence (i, (fromIntegral <$> randomRIO (0 :: Word, 0xffffff)) :: IO Word32) | i <- [1..iterations-1]] ++ [return (iterations, 0)] )

    window <- createWindow title sdlWindowPosCentered sdlWindowPosCentered (CInt $ fromIntegral width) (CInt $ fromIntegral height) 0
    surface <- getWindowSurface window
    lockSurface surface
    pixels <- liftM (castPtr.surfacePixels) $ peek surface :: IO (Ptr Word32)

    forM_ [0..height-1] (\y_ -> do
        forM_ [0..width-1] (\x_ -> do
            let x = 3/widthF * (fromIntegral x_) - 2
                y = -2/heightF * (fromIntegral y_) + 1
                mand = mandelbrotI (x :+ y)
            pokeElemOff pixels (x_ + y_*width) $ palette ! mand)
        updateWindowSurface window)

    unlockSurface surface
    updateWindowSurface window

    while (liftM not $ quitRequested) $ threadDelay 100000
