module LogBifurc where

import Data.Set
import Data.Fixed
import System.IO

lMap :: Double -> Double -> Double
lMap x r = (myRound 4) $ r * x * (1-x)

iter :: Double -> Double -> Int -> Double
iter x r n
     | n == 0    = x
     | otherwise = iter (lMap x r) r (n-1)

attractor :: Double -> Double -> [Double]
attractor x r =
    let x' = iter x r 100
        ts = singleton x'
    in toList $ go x' r 1000 ts

go :: Double -> Double -> Int -> Set Double -> Set Double
go x r n ts
   | n == 0    = ts
   | otherwise = let x'  = lMap x r
                     ts' = insert x' ts
                 in go x' r (n-1) ts'

singleR :: Double -> [(Double, Double)]
singleR r =
    let ts = attractor 0.1 r
        r' = myRound 4 r
    in fmap ((,) r') ts


--
-- Utility functions not related to the math
--
myRound :: Double -> Double -> Double
myRound n v  =
    let scale = 10.0 ** n
    in fromIntegral (truncate $ v * scale) / scale

showTup :: (Double, Double) -> String
showTup t = concat $ [show $ fst t, ",", show $ snd t, "\n"]

linspace :: Double -> Double -> Int -> [Double]
linspace start stop n =
    let step = findStep start stop (toInteger n)
    in scanl (+) 0 $ Prelude.take (n-1) $ repeat step

findStep :: Double -> Double -> Integer -> Double
findStep start stop n =
    let ninv = 1 / fromInteger (n - 1)
    in (stop - start) * ninv
