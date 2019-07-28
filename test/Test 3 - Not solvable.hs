module Main where

import Sudoku

main :: IO ()
main = test puzzle answer

-- not solveable
puzzle :: SudokuPuzzle
puzzle = GenPuzzle (3, 3) (GenBoard [
  5,0,0, 0,4,8, 3,0,0,
  0,0,0, 9,2,0, 5,0,0,
  2,4,1, 0,0,0, 9,0,7,

  1,0,0, 2,0,0, 0,0,0,
  0,0,7, 8,0,6, 0,4,0,
  3,0,8, 0,0,0, 6,5,9,

  8,7,0, 3,0,0, 0,0,5,
  0,0,2, 0,9,0, 8,7,1,
  9,0,5, 0,0,0, 2,6,0])

answer :: [SudokuPuzzle]
answer = []
