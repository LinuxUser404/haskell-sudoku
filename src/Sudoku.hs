-- Copyright (c) 2016, Mykola Pershyn
module Sudoku
    ( readPuzzle
    , SudokuPuzzle(..)
    , SudokuBoard(..)
    , solve
    , test
    , getPuzzleRow
    , getPuzzleCol
    , getPuzzleRect
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
data SudokuBoard  = GenBoard [Cell] deriving Eq
type Candidate = (Int, Cell) -- (pos, possibleValue)
type Candidates = (Int, [Cell]) -- (pos, possibleValues)
type Size = (Int, Int)
type Update = SudokuPuzzle -> SudokuPuzzle
type Cell = Int

test :: SudokuPuzzle -> [SudokuPuzzle] -> IO ()
test testPuzzle correctSolution = putStr (show testPuzzle) >> (putStr . showSolution) testSolution >> isPassed where
  testSolution = solve testPuzzle
  showSolution [] = "No solutions\n"
  showSolution [x] = show x
  showSolution xs = (show $ length xs) ++ " solutions found\n"
  isPassed  = if correctSolution /= testSolution then exitFailure else exitSuccess

instance Show SudokuPuzzle where
  show (GenPuzzle (x, y) (GenBoard board)) = unlines $ (unwords (map show [x, y])) : (map unwords . toRows . map showCell $ board) where
    toRows [] = []
    toRows b = (take (x * y) b) : (toRows $ drop (x * y) b)

solve :: SudokuPuzzle -> [SudokuPuzzle]
solve puzzle
  | isValid    puzzle == False   = []       -- cuts invalid answers, test4 has a unique solution
  | isFilled   puzzle            = [puzzle] -- solves test1
  | isSolvable puzzle == False   = []       -- solves test3
  | (hasSoleCandidates   puzzle) = solve . (flip updatePuzzle puzzle) . getSoleCandidates   $ puzzle -- solves test2
  | (hasUniqueCandidates puzzle) = solve . (flip updatePuzzle puzzle) . getUniqueCandidates $ puzzle -- still not enough for test4
  | otherwise                  = concat . map solve . derivePuzzles puzzle . head . getCandidates $ puzzle -- solves slowly(test4)
    where
      hasSoleCandidates = (>0) . length . getSoleCandidates

-- checks for duplicates that are > 0
isValid :: SudokuPuzzle -> Bool
isValid puzzle = all (==False) . map check $ [getPuzzleRow, getPuzzleCol, getPuzzleRect] where
  values = [1 .. n]
  setIDs = [0 .. n - 1]
  n = getPuzzleSize puzzle
  check getSet = any (==True) . map (any (>0) . (\\ values) . getSet puzzle) $ setIDs

isFilled :: SudokuPuzzle -> Bool
isFilled puzzle = all (>0) . getCells $ puzzle

isSolvable :: SudokuPuzzle -> Bool
isSolvable puzzle = filter (\val -> snd val == []) (getCandidates puzzle) == []


hasUniqueCandidates :: SudokuPuzzle -> Bool
hasUniqueCandidates puzzle = (length . getUniqueCandidates) puzzle > 0

-- constrains :: [(position, possibleValues)]
getSoleCandidates :: SudokuPuzzle -> [Candidate]
getSoleCandidates = filterSoleCandidates . getCandidates where
  filterSoleCandidates candidates = map ( \ (n, i:_) -> (n,i) ) . filter ((==1) .length . snd) $ candidates

derivePuzzles :: SudokuPuzzle -> Candidates -> [SudokuPuzzle]
derivePuzzles puzzle (pos, vals) = map (flip updatePuzzle puzzle) . map (\ i -> [(pos, i)]) $ vals

getCells :: SudokuPuzzle -> [Cell]
getCells (GenPuzzle _ (GenBoard board)) = board

getPuzzleDimX :: SudokuPuzzle -> Int
getPuzzleDimY :: SudokuPuzzle -> Int
getPuzzleSize :: SudokuPuzzle -> Int
getPuzzleDimX (GenPuzzle (x, _) _) = x
getPuzzleDimY (GenPuzzle (_, y) _) = y
getPuzzleSize (GenPuzzle (x, y) _) = x * y

getPuzzleRowIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleRowIndicies   puzzle              rowID  = map (\colID -> rowID * (getPuzzleSize puzzle) + colID) $ [0..(getPuzzleSize puzzle)-1]
getPuzzleColIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleColIndicies   puzzle              colID  = map (\rowID -> rowID * (getPuzzleSize puzzle) + colID) $ [0..(getPuzzleSize puzzle)-1]
getPuzzleRectIndicies :: SudokuPuzzle -> Int -> [Int]
getPuzzleRectIndicies (GenPuzzle (x, y) _) rectID = map (\rectElement -> getIndexOfElementInRectangle x y rectID rectElement) $ [0..x*y-1]
getPuzzleRow          :: SudokuPuzzle -> Int -> [Cell]
getPuzzleRow  puzzle rowID  = map (getCells puzzle !!) $ getPuzzleRowIndicies  puzzle rowID
getPuzzleCol          :: SudokuPuzzle -> Int -> [Cell]
getPuzzleCol  puzzle colID  = map (getCells puzzle !!) $ getPuzzleColIndicies  puzzle colID
getPuzzleRect         :: SudokuPuzzle -> Int -> [Cell]
getPuzzleRect puzzle rectID = map (getCells puzzle !!) $ getPuzzleRectIndicies puzzle rectID

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

cellIDToRowID :: Int -> SudokuPuzzle -> Int
cellIDToRowID cellID puzzle = cellID `quot` n where
  n = getPuzzleSize puzzle

--
cellIDToColID :: Int -> SudokuPuzzle -> Int
cellIDToColID cellID puzzle = cellID `rem` n where
  n = getPuzzleSize puzzle
--
cellIDToRectID :: Int -> SudokuPuzzle -> Int
cellIDToRectID cellID puzzle = rowID `quot` y * y + colID `quot` x where
  x = getPuzzleDimX puzzle
  y = getPuzzleDimY puzzle
  rowID = cellIDToRowID cellID puzzle
  colID = cellIDToColID cellID puzzle

-- returns a list of possible values for a specific cell, so each of those values do not contradict with any of the know values
getCandidate :: SudokuPuzzle -> Int -> Candidates
getCandidate puzzle cellID = (cellID, [1..(getPuzzleSize puzzle)] \\ presentValues)
  where
    presentValues = [] `union` set1 `union` set2 `union` set3
    set1 = getPuzzleRow  puzzle $ cellIDToRowID  cellID puzzle
    set2 = getPuzzleCol  puzzle $ cellIDToColID  cellID puzzle
    set3 = getPuzzleRect puzzle $ cellIDToRectID cellID puzzle

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
getUniqueCandidates puzzle = sort . union [] . concat . (map myVal) $ [1..size]
  where
    -- get indecies of a set, peek empty, get candidates for them, peek candidates with testVal
    -- in other words get candidates for a given set with testVal in them.
    constr indexFunction testVal = filter (elem testVal . snd) . map (getCandidate puzzle) . filterUnknows . indexFunction puzzle
    cells = getCells puzzle
    filterUnknows = filter $ (==0) . (!!) cells -- filter indecies of unknown cells
    fullConstr :: Int -> [[(Int, [Int])]] -- for each set get candidates with testVal
    fullConstr testVal = concat . map (myF testVal) $ [getPuzzleRowIndicies, getPuzzleColIndicies, getPuzzleRectIndicies]
    myF testVal indices = map (constr indices testVal) [0..size - 1] -- get candidates with val for each set.
    filterUnique = filter $ (==1) . length
    myVal :: Int -> [(Int, Int)] -- for each value get unique candidates
    myVal testVal = map (flip (,) testVal . fst . head) . filterUnique . fullConstr $ testVal
    size = getPuzzleSize puzzle

updatePuzzle :: [Candidate] -> Update
updatePuzzle theUpdates = foldl1 (.) $ map updateWith theUpdates where
  updateWith (pos, val) (GenPuzzle size (GenBoard oldBoard)) = GenPuzzle size (GenBoard newBoard) where
    newBoard = left ++ (val:right)
    (left, (0:right)) = splitAt pos oldBoard -- the update shouldn't happen on a filled cell

readCell :: String -> Cell
readCell s
  | s == "_"    = 0
  | otherwise   = read s :: Cell

showCell :: Cell -> String
showCell s
  | s == 0    = "_"
  | otherwise   = show s
