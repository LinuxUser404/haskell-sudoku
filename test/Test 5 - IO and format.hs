module Main where

import Sudoku

-- TODO: Check the format to fail if it is wrong.

main :: IO ()
main = do
  putStr $ show puzzle
  putStrLn $ "Row: "  ++ (show $ getSet Row  puzzle 0)
  putStrLn $ "Col: "  ++ (show $ getSet Col  puzzle 0)
  putStrLn $ "Rect: " ++ (show $ getSet Rect puzzle 0)
  return ()

-- 3x2 assymmetric puzzle for sets indexing
puzzle :: SudokuPuzzle
puzzle = GenPuzzle (3, 2) (GenBoard $ take 36 [1..])
