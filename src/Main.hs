module Main where

import Board
import Player
import System.Exit
import Control.Monad

main :: IO ()
main = play emptyBoard [Player X, Player O]

play :: Board -> [Player] -> IO ()
play b (p1:p2:_) = do
  print b
  newBoard <- makeMove b p1

  case boardWon newBoard of
    Just winningCell ->
      handleWinner newBoard winningCell
    Nothing ->
      if boardTie newBoard
        then handleTie newBoard
        else handleNextMove newBoard p1 p2


makeMove :: Board -> Player -> IO Board
makeMove b (Player c) = do
  input <- getInputForMove b c
  let newBoard = updateBoard b c input
  return newBoard

getInputForMove :: Board -> Cell -> IO Int
getInputForMove b c = do
  print $ "Player " ++ show c ++ ", what is your move?"
  input <- liftM read getLine
  if validMove input
    then return input
    else getInputForMove b c
  where
  validMove input = input >= 0 && input <= 8 && emptyCell b input

handleWinner :: Board -> Winner -> IO ()
handleWinner b c = do
  putStrLn $ "Board was Won by " ++ show c ++ "!"
  print b
  exitSuccess

handleTie :: Board -> IO ()
handleTie b = do
  putStrLn "It's a tie! Play again soon!"
  print b

handleNextMove :: Board -> Player -> Player -> IO ()
handleNextMove b p1 p2 = do
  putStrLn "Board is not won yet..."
  play b [p2, p1]
