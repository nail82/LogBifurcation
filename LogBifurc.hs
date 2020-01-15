module LogBifurc where

import Data.Set
import Data.Fixed
import System.IO

-- Logistic map
lMap :: Double -> Double -> Double
lMap x r = (myRound 4) $ r * x * (1-x)

-- Iterate lMap
iter :: Double -> Double -> Int -> Double
iter x r n
     | n == 0    = x
     | otherwise = iter (lMap x r) r (n-1)

-- Run 1100 iterations of lMap, and collect the
-- attractors into a set, then convert the set to a list.
attractor :: Double -> Double -> [Double]
attractor x r =
    let x' = iter x r 100
        ts = singleton x'
    in toList $ go x' r 1000 ts

-- Recursion helper for attractor
go :: Double -> Double -> Int -> Set Double -> Set Double
go x r n ts
   | n == 0    = ts
   | otherwise = let x'  = lMap x r
                     ts' = insert x' ts
                 in go x' r (n-1) ts'

-- For a given r value, find the associated attractors
-- and pair them up with r, for later analysis / plotting.
singleR :: Double -> [(Double, Double)]
singleR r =
    let ts = attractor 0.1 r
        r' = myRound 4 r
    in fmap ((,) r') ts


--
-- Utility functions not related to the math
--

-- A rounding function.  A similar one is in Data.Astro.Utils
myRound :: Double -> Double -> Double
myRound n v  =
    let scale = 10.0 ** n
    in fromIntegral (truncate $ v * scale) / scale

showTup :: (Double, Double) -> String
showTup t = concat $ [show $ fst t, ",", show $ snd t]

linspace :: Double -> Double -> Int -> [Double]
linspace start stop n =
    let step = findStep start stop (toInteger n)
    in scanl (+) 0 $ Prelude.take (n-1) $ repeat step

findStep :: Double -> Double -> Integer -> Double
findStep start stop n =
    let ninv = 1 / fromInteger (n - 1)
    in (stop - start) * ninv
