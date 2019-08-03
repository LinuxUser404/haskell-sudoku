-- Copyright (c) 2016, Mykola Pershyn
module Main where

--import System.IO
--import Data.List
-- TODO: Client/Server
-- import Network.HTTP.Client.Request
-- TODO: more IO options

import Sudoku

main :: IO ()
main = putStr . toString . solve =<< getSudoku where
  getSudoku = do
    dimensions   <- getLine
    readPuzzle dimensions <$> getContents

toString :: [SudokuPuzzle] -> String
toString [] = "There are no solutions"
toString [s] = show s
toString solutions = "There are " ++ show (length solutions) ++ " solutions:\n" ++ concatMap show solutions
