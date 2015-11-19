{-# LANGUAGE ScopedTypeVariables #-}

module Mandelbrot
    ( mandelbrot
    ) where

import Data.Complex
import Debug.Trace

mag2 :: RealFloat a => Complex a -> a
mag2 z = (realPart z)**2 + (imagPart z)**2

-- For given point and maxIterations returns iteration number when diverged
mandelbrot :: forall a. (RealFloat a, Show a) => Int -> Complex a -> Int
mandelbrot maxIter c = mandelbrot_ 0 0
    where
        mandelbrot_ :: Int -> Complex a -> Int
        mandelbrot_ iter z0 =
            if iter >= maxIter || (mag2 z0) > 4
            then iter
            else mandelbrot_ (iter+1) (z0*z0 + c)
