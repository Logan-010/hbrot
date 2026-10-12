module Main (main) where

import qualified Brot
import qualified Cli
import Codec.Picture (DynamicImage (ImageRGB8), Image (..), PixelRGB8 (..), generateImage, saveJpgImage)
import qualified Colormaps
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import qualified Data.Foldable
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import GHC.Float (powerDouble)
import System.Environment (getArgs)
import System.Random (mkStdGen, randomR)
import Text.Printf (printf)

randChoice :: [a] -> Int -> a
randChoice xs se = xs !! fst (randomR (0, length xs - 1) (mkStdGen se))

formatDuration :: Word64 -> String
formatDuration nanos
  | nanos < 1000 = show nanos ++ "ns"
  | nanos < 1000000 = withUnit 1000 3 "µs"
  | nanos < 1000000000 = withUnit 1000000 6 "ms"
  | otherwise = withUnit 1000000000 9 "s"
  where
    withUnit :: Word64 -> Int -> String -> String
    withUnit divisor digits unit =
      let (whole, remainder) = nanos `quotRem` divisor
          fraction = reverse (dropWhile (== '0') (reverse (printf "%0*d" digits remainder)))
       in show whole ++ (if remainder == 0 then "" else "." ++ fraction) ++ unit

renderImage :: Int -> (Int, Int) -> (Double, Double) -> (Double, Double) -> Brot.Config -> [Integer] -> Image PixelRGB8
renderImage capabilities (width, height) (xmin, xmax) (ymin, ymax) cfg colormap =
  let palette = V.fromList (map fromIntegral colormap) :: V.Vector Word8
      fraction position size = if size == 1 then 0 else fromIntegral position / fromIntegral (size - 1)
      pixel x y =
        let xf = fraction x width
            yf = fraction y height
            iteration = Brot.mandelbrot (Brot.lerp xmin xmax xf, Brot.lerp ymin ymax yf) cfg
            idx = max 0 (min (3 * iteration) (V.length palette - 3))
         in PixelRGB8 (palette V.! idx) (palette V.! (idx + 1)) (palette V.! (idx + 2))
      rows =
        [imageData (generateImage (\x _ -> pixel x y) width 1) | y <- [0 .. height - 1]]
          `using` parBuffer (2 * capabilities) rdeepseq
   in Image width height (V.concat rows)

main :: IO ()
main = do
  getArgs >>= \args -> case Cli.parse args of
    Left err -> putStrLn err
    Right cli ->
      maybe Brot.newSeed pure (Cli.rngSeed cli)
        >>= \seed ->
          let cfg = Brot.Config {Brot.steps = Cli.stepLimits cli, Brot.bailout = powerDouble 10 $ Cli.bailoutNum cli}
              colormap = case Cli.colormap cli of
                Just c -> Colormaps.toColormap c
                Nothing -> Colormaps.toColormap $ randChoice Colormaps.colormapChoices seed
              ((cx, cy), steps) = case Cli.imageCenter cli of
                Just p -> (p, Brot.mandelbrot p cfg)
                Nothing -> Brot.chooseCenter cfg seed
              (dx, dy) = case Cli.viewSize cli of
                Just (w, h) -> (w / 2, h / 2)
                Nothing ->
                  let dx' = fromIntegral (max 1 steps) ** Brot.randRange (-2.5) (-1.0) seed
                   in (dx', dx' * fromIntegral (snd $ Cli.dimensions cli) / fromIntegral (fst $ Cli.dimensions cli))
              (xrange, yrange) = ((cx - dx, cx + dx), (cy - dy, cy + dy))
           in do
                 Data.Foldable.for_ (Cli.threads cli) setNumCapabilities
                 capabilities <- getNumCapabilities
                 _ <- evaluate (force (xrange, yrange))
                 unless (Cli.noInfo cli) $
                   printf "Starting generation with seed %d...\n" seed
                 timer <- getMonotonicTimeNSec
                 let image = renderImage capabilities (Cli.dimensions cli) xrange yrange cfg colormap
                 _ <- evaluate (force (imageData image))
                 finished <- getMonotonicTimeNSec
                 unless (Cli.noInfo cli) $
                   putStrLn ("Finished generation in " ++ formatDuration (finished - timer) ++ "!\nSaving image...")
                 saveJpgImage 95 (Cli.fileName cli) (ImageRGB8 image)
                 unless (Cli.noInfo cli) $ putStrLn "Done!"
