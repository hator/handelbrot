import Mandelbrot
import Data.Complex (Complex((:+)))
import Data.Array.MArray
import Data.Array.Storable
import Data.Word (Word32)
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Criterion.Measurement
import Criterion.Types (Benchmarkable(..))
import Data.Int (Int64)

mandelbrotI = mandelbrot 10

mandelbrotArray :: Int -> Int -> IO ()
mandelbrotArray height width = do
    let widthF = fromIntegral width :: Double
        heightF = fromIntegral height :: Double
    array <- newArray_ (0, height*width-1) :: IO (StorableArray Int Word32)
    withStorableArray array (\pixels ->
        forM_ [0..height-1] (\y_ -> do
            forM_ [0..width-1] (\x_ -> do
                let x = 3/widthF * (fromIntegral x_) - 2
                    y = -2/heightF * (fromIntegral y_) + 1
                    mand = mandelbrotI (x :+ y)
                    color = if mand == 100 then 0 else 0xffffff
                pokeElemOff pixels (x_ + y_*width) color)))

simpleBenchmark :: Benchmarkable
simpleBenchmark = Benchmarkable (\tries ->
    forM_ [1..tries] (\_ ->
        mandelbrotArray 100 100))

main :: IO ()
main = secs <$> snd <$> measure simpleBenchmark 1 >>= printLn
