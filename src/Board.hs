module Board where

import Prelude hiding (replicate)
import Control.Monad hiding (replicateM)
import System.Random
import Data.Vector hiding ( (++), map )
import Data.List (intersperse, intercalate)
import Data.List.Split (chunksOf)

data Cell = X | O | EmptyCell
newtype Board = Board { getBoard :: Vector Cell }

instance Show Cell where
  show (X)         = "X"
  show (O)         = "O"
  show (EmptyCell) = " "

  -- See https://gist.github.com/reinh/85fca4b3a516ef839992 for ReinH's pointfree style suggestion
instance Show Board where
  show b = (unlines . withLines . withPipes) eachLine
    where
      eachLine = chunksOf 3 $ (map show . toList . getBoard) b
      withPipes = map ((" " ++) . intercalate " | ")
      withLines = intersperse "----------"

emptyBoard :: Board
emptyBoard = Board (replicate 9 EmptyCell)

randomBoard :: IO Board
randomBoard = liftM Board (replicateM 9 makeRandomCell)
  where
  makeRandomCell :: IO Cell
  makeRandomCell = do
    r <- getStdRandom $ randomR (1 :: Int, 3)
    return $ case r of
      1 -> X
      2 -> O
      3 -> EmptyCell
