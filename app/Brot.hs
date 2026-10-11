module Brot (Config (..), mandelbrot, chooseCenter, lerp) where

import Data.Bifunctor (bimap)
import System.Random (randomRIO)

data Config = Config
  { steps :: (Int, Int),
    bailout :: Double
  }
  deriving (Show, Eq)

mandelbrot' :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Int -> Config -> Int
mandelbrot' (c0Re, c0Im) (cRe, cIm) (dRe, dIm) (sRe, sIm) n c =
  let c2 = (cRe * cRe - cIm * cIm + c0Re, 2 * cRe * cIm + c0Im)
   in let d2 = (2 * (dRe * fst c2 - dIm * snd c2) + 1, 2 * (dRe * snd c2 + dIm * fst c2))
       in let s2 = bimap (sRe +) (sIm +) d2
           in let mag2 = fst s2 * fst s2 + snd s2 * snd s2
               in if mag2 >= bailout c
                    then n
                    else
                      if n >= snd (steps c) - 1
                        then 0
                        else mandelbrot' (c0Re, c0Im) c2 d2 s2 (n + 1) c

mandelbrot :: (Double, Double) -> Config -> Int
mandelbrot point = mandelbrot' point point (1, 0) (0, 0) 1

lerp :: (Num a) => a -> a -> a -> a
lerp a b u = a * (1 - u) + b * u

randRange :: Double -> Double -> IO Double
randRange mi ma = do
  randomRIO (0.0, 1.0) >>= \u ->
    pure (lerp mi ma u)

chooseCenter' :: Config -> (Double, Double) -> Int -> IO ((Double, Double), Int)
chooseCenter' c (x, y) n =
  if n < fst (steps c) || n > snd (steps c)
    then pure ((x, y), n)
    else
      randRange (-1.5) 1 >>= \nx ->
        randRange 0 1 >>= \ny ->
          chooseCenter' c (nx, ny) (n + 1)

chooseCenter :: Config -> IO ((Double, Double), Int)
chooseCenter c =
  randRange (-1.5) 1 >>= \x ->
    randRange 0 1 >>= \y ->
      chooseCenter' c (x, y) 0
