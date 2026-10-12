module Main (main) where

import qualified Brot
import qualified Cli
import qualified Colormaps
import GHC.Float (powerDouble)
import System.Environment (getArgs)
import System.Random (mkStdGen, randomR)

randChoice :: [a] -> Int -> a
randChoice xs se = xs !! fst (randomR (0, length xs - 1) (mkStdGen se))

main :: IO ()
main = do
  getArgs >>= \args -> case Cli.parse args of
    Left err -> putStrLn err
    Right cli ->
      ( case Cli.rngSeed cli of
          Just s -> pure s
          Nothing -> Brot.newSeed
      )
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
                  let dx' = fromIntegral steps ** Brot.randRange (-2.5) (-1.0) seed
                   in (dx', dx' * fromIntegral (snd $ Cli.dimensions cli) / fromIntegral (fst $ Cli.dimensions cli))
              (xrange, yrange) = ((cx - dx, cx + dx), (cy - dy, cy + dy))
           in pure ()
