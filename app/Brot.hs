module Brot (Config (..), mandelbrot, chooseCenter, lerp, newSeed, randRange) where

import Data.Bifunctor (bimap)
import System.Random (Random (randomR), mkStdGen, randomIO)

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

randRange :: Double -> Double -> Int -> Double
randRange mi ma se = lerp mi ma . fst . randomR (0.0, 1.0) $ mkStdGen se

newSeed :: IO Int
newSeed = randomIO

chooseCenter' :: Config -> (Double, Double) -> Int -> Int -> ((Double, Double), Int)
chooseCenter' c (x, y) n s =
  if n < fst (steps c) || n > snd (steps c)
    then ((x, y), n)
    else
      let nx = randRange (-1.5) 1 s
       in let ny = randRange 0 1 s
           in chooseCenter' c (nx, ny) (n + 1) s

chooseCenter :: Config -> Int -> ((Double, Double), Int)
chooseCenter c s =
  let x = randRange (-1.5) 1 s
   in let y = randRange 0 1 s
       in chooseCenter' c (x, y) 0 s
