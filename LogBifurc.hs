module LogBifurc where

import Data.Set
import Data.Fixed

lMap :: Milli -> Milli -> Milli
lMap x r = r * x * (1-x)

iter :: Milli -> Milli -> Int -> Milli
iter x r n
     | n == 0    = x
     | otherwise = iter (lMap x r) r (n-1)

attractor :: Milli -> Milli -> [Milli]
attractor x r =
    let x' = iter x r 100
        ts = singleton x'
    in toList $ go x' r 1000 ts

go :: Milli -> Milli -> Int -> Set Milli -> Set Milli
go x r n ts
   | n == 0    = ts
   | otherwise = let x'  = lMap x r
                     ts' = insert x' ts
                 in go x' r (n-1) ts'

linspace :: Milli -> Milli -> Int -> [Milli]
linspace start stop n =
    let step = findStep start stop (toInteger n)
    in scanl (+) 0 $ Prelude.take (n-1) $ repeat step

findStep :: Milli -> Milli -> Integer -> Milli
findStep start stop n =
    let ninv = 1 / fromInteger (n - 1)
    in (stop - start) * ninv

singleR :: Milli -> [(Milli, Milli)]
singleR r =
    let ts = attractor 0.1 r
    in fmap ((,) r) ts

myRound :: Double -> Double -> Double
myRound n v  =
    let scale = 10.0 ** n
    in fromIntegral (truncate $ v * scale) / scale
