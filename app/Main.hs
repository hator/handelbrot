module Main where

import Mandelbrot
import Data.Complex (Complex((:+)))

main :: IO ()
main = print $ show $ mandelbrot 10 (0 :+ 0)
