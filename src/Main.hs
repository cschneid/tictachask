module Main where

import Board

main :: IO ()
main = do
  b <- randomBoard
  print b
