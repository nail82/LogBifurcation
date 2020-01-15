module Main where

import System.IO
import LogBifurc
import Data.List

main :: IO ()
main = do
  let
      fnm = "test1.csv"
      rs = linspace 0 4 1000
      ts = concat $ fmap singleR rs
      num = length ts
      lines = intercalate "\n" $ ["r,val"] ++ fmap showTup ts
  fh <- openFile fnm WriteMode
  hPutStrLn fh lines
  hClose fh
  putStrLn $ "Wrote " ++ show num ++ " tuples."
