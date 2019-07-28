module Main where

import Sudoku

main :: IO ()
main = test puzzle answer

-- already solved
puzzle :: SudokuPuzzle
puzzle = GenPuzzle (3, 2) (GenBoard [
  1,2,3, 4,5,6,
  4,5,6, 1,2,3,
  2,3,4, 5,6,1,
  5,6,1, 2,3,4,
  3,4,5, 6,1,2,
  6,1,2, 3,4,5])

answer :: [SudokuPuzzle]
answer = [GenPuzzle (3, 2) (GenBoard [
  1,2,3, 4,5,6,
  4,5,6, 1,2,3,
  2,3,4, 5,6,1,
  5,6,1, 2,3,4,
  3,4,5, 6,1,2,
  6,1,2, 3,4,5])]
