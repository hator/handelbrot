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
import Data.Int (Int32(..))
import Data.Word
import Control.Monad
import Control.Concurrent (threadDelay)

sdlWindowPosCentered :: CInt
sdlWindowPosCentered = 0x2fff0000

width :: Int
width = 1024

height :: Int
height = 786

widthF :: Double
widthF = fromIntegral width

heightF :: Double
heightF = fromIntegral height

mandelbrotI :: RealFloat a => Complex a -> Int
mandelbrotI = mandelbrot 100

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
    window <- createWindow title sdlWindowPosCentered sdlWindowPosCentered (CInt $ fromIntegral width) (CInt $ fromIntegral height) 0
    surface <- getWindowSurface window
    lockSurface surface
    pixels <- liftM (castPtr.surfacePixels) $ peek surface :: IO (Ptr Word32)
    forM_ [0..height-1] (\y_ -> do
        forM_ [0..width-1] (\x_ -> do
            let x = 3/widthF * (fromIntegral x_) - 2
                y = -2/heightF * (fromIntegral y_) + 1
                mand = mandelbrotI (x :+ y)
                color = if mand == 100 then 0 else 0xffffff
            pokeElemOff pixels (x_ + y_*width) color)
        updateWindowSurface window)
    unlockSurface surface
    while (liftM not $ quitRequested) $ threadDelay 100000

