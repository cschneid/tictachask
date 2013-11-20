module Board where

import Control.Monad
import System.Random
import Data.Vector ( Vector, (!), (//), generate, generateM, toList )
import Data.List (intersperse, intercalate)
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.Maybe

data Cell = X | O | EmptyCell Int
data Winner = Winner Cell
newtype Board = Board { getBoard :: Vector Cell }

instance Show Cell where
  show (X)           = "X"
  show (O)           = "O"
  show (EmptyCell i) = show i

instance Eq Cell where
  X == X = True
  O == O = True
  _ == _ = False

instance Show Winner where
  show (Winner c) = show c

-- See https://gist.github.com/reinh/85fca4b3a516ef839992 for ReinH's pointfree style suggestion
instance Show Board where
  show b = (unlines . withLines . withPipes) eachLine
    where
      eachLine = chunksOf 3 $ (map show . toList . getBoard) b
      withPipes = map ((" " ++) . intercalate " | ")
      withLines = intersperse "----------"

emptyBoard :: Board
emptyBoard = Board (generate 9 EmptyCell)

randomBoard :: IO Board
randomBoard = liftM Board (generateM 9 makeRandomCell)
  where
  makeRandomCell :: Int -> IO Cell
  makeRandomCell i = do
    r <- getStdRandom $ randomR (1 :: Int, 3)
    return $ case r of
      1 -> X
      2 -> O
      3 -> EmptyCell i

-- Nothing if not won, otherwise winning cell is returned
boardWon :: Board -> Maybe Winner
boardWon b = getFirst $ mconcat $ map (First . won b) winningLines
  where
    won :: Board -> [Int] -> Maybe Winner
    won (Board cells) line = if allSame $ map (cells !) line
                             then Just $ Winner $ cells ! head line
                             else Nothing

boardTie :: Board -> Bool
boardTie b = allCellsFilled b && isNothing (boardWon b)
  where
    allCellsFilled (Board cs) = all filled (toList cs)
    filled c = case c of
      EmptyCell _ -> False
      _           -> True

allSame :: Eq a => [a] -> Bool
allSame xs = all (== head xs) (tail xs)

-- All possible winning paths through the board
winningLines :: [[Int]]
winningLines = [ [0,1,2] -- Horizontals
               , [3,4,5]
               , [6,7,8]
               , [0,3,6] -- Verticals
               , [1,4,7]
               , [2,5,8]
               , [0,4,8] -- Diagonals
               , [2,4,6]
               ]

-- Set a single cell to the new value
updateBoard :: Board -> Cell -> Int -> Board
updateBoard (Board cells) c i = Board $ cells // [(i, c)]

-- True if the cell is currently empty
emptyCell :: Board -> Int -> Bool
emptyCell (Board cells) i = case cells ! i of
                              (EmptyCell _) -> True
                              _             -> False
