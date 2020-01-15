module Main where

import System.IO
import LogBifurc

main :: IO ()
main = do
  let
      fnm = "test1.csv"
      rs = linspace 0 4 1000
      ts = concat $ fmap singleR rs
      lines = concat $ ["r,", "val\n"] ++ fmap showTup ts
  fh <- openFile fnm WriteMode
  hPutStrLn fh lines
  hClose fh
