-- Copyright (c) 2016, Mykola Pershyn
{-# LANGUAGE TupleSections #-}
module Sudoku
    ( readPuzzle
    , SudokuPuzzle(..)
    , SudokuBoard(..)
    , solve
    , test
    , getSet
    , SetType(..)
    ) where

import Data.List
--import Data.Bits
import System.Exit(exitFailure, exitSuccess)

-- TODO: use arrays instead? bit?
-- import Data.Array

-- Utility functions
--myDot a b c = a c . b c
--myDot = liftM2 (.)

data SudokuPuzzle = GenPuzzle Size SudokuBoard deriving Eq
newtype SudokuBoard  = GenBoard [Cell] deriving Eq
type Candidate = (Int, Cell) -- (pos, possibleValue)
type Candidates = (Int, [Cell]) -- (pos, possibleValues)
type Size = (Int, Int)
type Update = SudokuPuzzle -> SudokuPuzzle
type Cell = Int
--type CandidateSet = [Candidates] -- a set of candidates within a single row/col/rect
data SetType = Row | Col | Rect

test :: SudokuPuzzle -> [SudokuPuzzle] -> IO ()
test testPuzzle correctSolution = if correctSolution == testSolution then passed else failed where
  passed = exitSuccess
  failed = putStr (show testPuzzle) >> (putStr . showSolution) testSolution >> exitFailure
  testSolution = solve testPuzzle
  showSolution [] = "No solutions\n"
  showSolution [x] = show x
  showSolution xs = show (length xs) ++ " solutions found, expected: " ++ show (length correctSolution)

instance Show SudokuPuzzle where
  show (GenPuzzle (x, y) (GenBoard board)) = unlines $ unwords (map show [x, y]) : (map unwords . toRows . map showCell $ board) where
    toRows [] = []
    toRows b = take (x * y) b : toRows (drop (x * y) b)

solve :: SudokuPuzzle -> [SudokuPuzzle]
solve puzzle
  | not (isValid    puzzle)    = []       -- cuts invalid answers, test4 has a unique solution
  |      isFilled   puzzle     = [puzzle] -- solves test1
  | not (isSolvable puzzle)    = []       -- solves test3
  | hasSoleCandidates   puzzle = solve . (`updatePuzzle` puzzle) . getSoleCandidates   $ puzzle -- solves test2
  | hasUniqueCandidates puzzle = solve . (`updatePuzzle` puzzle) . getUniqueCandidates $ puzzle -- still not enough for test4
  | otherwise                  = concatMap solve . derivePuzzles puzzle . head . getCandidates $ puzzle -- solves slowly(test4)
    where
      hasSoleCandidates = not . null . getSoleCandidates
      hasUniqueCandidates = not . null . getUniqueCandidates

-- checks for duplicates in a partially filled sudoku
isValid :: SudokuPuzzle -> Bool
isValid puzzle = all (all (==0) . (\\ [1 .. n])) sets  where
  sets = concatMap (\s -> map (getSet s puzzle) setIDs) [Row, Col, Rect]
  setIDs = [0 .. n - 1]
  n = getPuzzleSize puzzle

isFilled :: SudokuPuzzle -> Bool
isFilled = all (>0) . getCells

-- every empty cell should have a candidate
isSolvable :: SudokuPuzzle -> Bool
isSolvable puzzle = not (any (null . snd) (getCandidates puzzle))

getSoleCandidates :: SudokuPuzzle -> [Candidate]
getSoleCandidates = filterSoleCandidates . getCandidates where
  filterSoleCandidates = map ( \ (n, i:_) -> (n,i) ) . filter ((==1) .length . snd)

derivePuzzles :: SudokuPuzzle -> Candidates -> [SudokuPuzzle]
derivePuzzles puzzle (pos, vals) = map (flip updatePuzzle puzzle . (\i -> [(pos, i)])) vals

getCells :: SudokuPuzzle -> [Cell]
getCells (GenPuzzle _ (GenBoard board)) = board

getPuzzleDimX :: SudokuPuzzle -> Int
getPuzzleDimY :: SudokuPuzzle -> Int
getPuzzleSize :: SudokuPuzzle -> Int
getPuzzleDimX (GenPuzzle (x, _) _) = x
getPuzzleDimY (GenPuzzle (_, y) _) = y
getPuzzleSize (GenPuzzle (x, y) _) = x * y

getSetIndicies :: SetType -> SudokuPuzzle -> Int -> [Int]
getSetIndicies Row   puzzle              rowID  = map (\colID -> rowID * getPuzzleSize puzzle + colID) [0.. getPuzzleSize puzzle - 1]
getSetIndicies Col   puzzle              colID  = map (\rowID -> rowID * getPuzzleSize puzzle + colID) [0.. getPuzzleSize puzzle - 1]
getSetIndicies Rect (GenPuzzle (x, y) _) rectID = map (getIndexOfElementInRectangle x y rectID) [0..x*y-1]

getSet :: SetType -> SudokuPuzzle -> Int -> [Cell]
getSet s puzzle setID = map (getCells puzzle !!) $ getSetIndicies s puzzle setID

getIndexOfElementInRectangle :: Int -> Int -> Int -> Int -> Int
getIndexOfElementInRectangle x y rectNum rectElement = rowNum * size + colNum
  where
    size   = x * y
    rowNum = rectNum `quot` y * y + rectElement `quot` x
    colNum = rectNum `rem`  y * x + rectElement `rem`  x

readPuzzle :: String -> String -> SudokuPuzzle
readPuzzle dimensions square = GenPuzzle (x, y) board
  where
    [x, y] = map read . words $ dimensions
    board = (GenBoard . map readCell . words) square

cellIDToSetID :: SetType -> Int -> SudokuPuzzle -> Int
cellIDToSetID Row  cellID puzzle = cellID `quot` getPuzzleSize puzzle
cellIDToSetID Col  cellID puzzle = cellID `rem`  getPuzzleSize puzzle
cellIDToSetID Rect cellID puzzle = rowID `quot` y * y + colID `quot` x
  where
    x = getPuzzleDimX puzzle
    y = getPuzzleDimY puzzle
    rowID = cellIDToSetID Row cellID puzzle
    colID = cellIDToSetID Col cellID puzzle

-- returns a list of possible values for a specific cell, so each of those values does not contradict with the knowns
getCandidate :: SudokuPuzzle -> Int -> Candidates
getCandidate puzzle cellID = (cellID, [1..(getPuzzleSize puzzle)] \\ presentValues)
  where
    presentValues = [] `union` set1 `union` set2 `union` set3
    [set1, set2, set3] = map(\s -> getSet s  puzzle $ cellIDToSetID s cellID puzzle) [Row, Col, Rect]

-- Get candidates for all cells that are not filled yet
getCandidates :: SudokuPuzzle -> [Candidates]
getCandidates puzzle = map (getCandidate puzzle) candidateCellIds
  where
    size = getPuzzleSize puzzle
    cells = getCells puzzle
    candidateCellIds = filter (\val -> (cells !! val) == 0) cellIDs
    cellIDs = [0..size*size - 1]

-- for each row/col/rect checks whether a specific value can be assigned only to one cell in a unit
getUniqueCandidates :: SudokuPuzzle -> [Candidate]
getUniqueCandidates puzzle = sort . union [] . concatMap myVal $ [1..size]
  where
    -- get indecies of a set, peek empty, get candidates for them, peek candidates with testVal
    -- in other words get candidates for a given set with testVal in them.
    constr indexSet testVal = filter (elem testVal . snd) . map (getCandidate puzzle) . filterUnknows . indexSet
    cells = getCells puzzle
    filterUnknows = filter $ (==0) . (!!) cells -- filter indecies of unknown cells
    fullConstr :: Int -> [[(Int, [Int])]] -- for each set get candidates with testVal
    fullConstr testVal = concatMap (myF testVal) [getSetIndicies Row  puzzle, getSetIndicies Col puzzle, getSetIndicies Rect puzzle]
    myF testVal indices = map (constr indices testVal) [0..size - 1] -- get candidates with val for each set.
    filterUnique = filter $ (==1) . length
    myVal :: Int -> [(Int, Int)] -- for each value get unique candidates
    myVal testVal = map ((, testVal) . fst . head) . filterUnique . fullConstr $ testVal
    size = getPuzzleSize puzzle

updatePuzzle :: [Candidate] -> Update
updatePuzzle theUpdates = foldl1 (.) $ map updateWith theUpdates where
  updateWith (pos, val) (GenPuzzle size (GenBoard oldBoard)) = GenPuzzle size (GenBoard newBoard) where
    newBoard = left ++ (val:right)
    (left, 0:right) = splitAt pos oldBoard -- the update shouldn't happen on a filled cell

readCell :: String -> Cell
readCell s
  | s == "_"    = 0
  | otherwise   = read s :: Cell

showCell :: Cell -> String
showCell s
  | s == 0    = "_"
  | otherwise   = show s
