module Main where

import Sudoku

main :: IO ()
main = test puzzle answer

-- solvable, but not solved
puzzle :: SudokuPuzzle
puzzle = GenPuzzle (3, 3) (GenBoard [
  0,0,0, 0,4,8, 3,0,0,
  0,0,0, 9,2,0, 5,0,0,
  2,4,1, 0,0,0, 9,0,7,

  1,0,0, 2,0,0, 0,0,0,
  0,0,7, 8,0,6, 0,4,0,
  3,0,8, 0,0,0, 6,5,9,

  8,7,0, 3,0,0, 0,0,5,
  0,0,2, 0,9,0, 8,7,1,
  9,0,5, 0,0,0, 2,6,0])

answer :: [SudokuPuzzle]
answer = [GenPuzzle (3, 3) (GenBoard [
  7,5,9, 1,4,8, 3,2,6,
  6,8,3, 9,2,7, 5,1,4,
  2,4,1, 5,6,3, 9,8,7,

  1,6,4, 2,5,9, 7,3,8,
  5,9,7, 8,3,6, 1,4,2,
  3,2,8, 4,7,1, 6,5,9,

  8,7,6, 3,1,2, 4,9,5,
  4,3,2, 6,9,5, 8,7,1,
  9,1,5, 7,8,4, 2,6,3])]
