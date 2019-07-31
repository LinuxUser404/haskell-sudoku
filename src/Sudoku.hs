-- Copyright (c) 2016, Mykola Pershyn

module Sudoku
    ( doMyMagic
    , SudokuPuzzle(..)
    , SudokuBoard(..)
    , solve
    , test
    ) where

import Data.List
--import Data.Bits
import System.Exit(exitFailure, exitSuccess)

-- Utility functions
--myDot a b c = a c . b c
--myDot = liftM2 (.)

data SudokuPuzzle = GenPuzzle Size SudokuBoard deriving Eq
data SudokuBoard  = GenBoard [Int] deriving Eq
--data Candidate = OneOf [Int]
type Candidate = (Int, [Int]) -- (pos, possibleValues)
type Size = (Int, Int)
type Update = SudokuPuzzle -> SudokuPuzzle
--type Cell = [Int]

defaultFormat :: [String]
defaultFormat = []

test :: SudokuPuzzle -> [SudokuPuzzle] -> IO ()
test testPuzzle correctSolution = putStr (show testPuzzle) >> (putStr . showSolution) testSolution >> isPassed where
  testSolution = solve testPuzzle
  showSolution [] = "No solutions\n"
  showSolution [x] = show x
  showSolution xs = (show $ length xs) ++ " solutions found\n"
  isPassed  = if correctSolution /= testSolution then exitFailure else exitSuccess


doMyMagic :: String -> String -> [String]
doMyMagic dimmensions square = ((map (\i -> (buildFormatedStringFromPuzzle i defaultFormat))) . solve . buildPuzzleFromFormatedString dimmensions) square

instance Show SudokuPuzzle where
  show (GenPuzzle (x, y) (GenBoard board)) = unlines $ (unwords (map show [x, y])) : (map unwords . format . map show $ board) where
    format [] = []
    format b = (take (x * y) b) : (format $ drop (x * y) b)

solve :: SudokuPuzzle -> [SudokuPuzzle]
solve puzzle
  | isValid    puzzle == False   = []       -- cuts invalid answers, test4 has a unique solution
  | isFilled   puzzle            = [puzzle] -- solves test1
  | isSolvable puzzle == False   = []       -- solves test3
  | (hasSoleCandidates   puzzle) = solve . (flip updatePuzzle puzzle) . getSoleCandidates         $ puzzle -- solves test2
  | (isLevelTwoUpdatable puzzle) = solve . (flip updatePuzzle puzzle) . getLevelTwoUnitConstrains $ puzzle -- still not enough for test4
  | otherwise                  = concat . map solve . derivePuzzles puzzle . head . getCandidates $ puzzle  -- solves slowly(test4)
    where
      hasSoleCandidates = (>0) . length . getSoleCandidates

isValid :: SudokuPuzzle -> Bool
isValid puzzle
  | check getPuzzleRow  = False
  | check getPuzzleCol  = False
  | check getPuzzleRect = False
  | otherwise = True
    where
      values = [1 .. n]
      setIDs = [0 .. n - 1]
      n = getPuzzleSize puzzle
      check getSet = any (==True) . map (\i -> any(>0) $ (getSet puzzle i) \\ values) $ setIDs

isFilled :: SudokuPuzzle -> Bool
isFilled puzzle = all (>0) . getCells $ puzzle

isSolvable :: SudokuPuzzle -> Bool
isSolvable puzzle = filter (\val -> snd val == []) (getCandidates puzzle) == []


isLevelTwoUpdatable :: SudokuPuzzle -> Bool
isLevelTwoUpdatable puzzle = (length . getLevelTwoUnitConstrains) puzzle > 0

-- constrains :: [(position, possibleValues)]
getSoleCandidates :: SudokuPuzzle -> [(Int, Int)]
getSoleCandidates = getSoleCandidates' . getCandidates where
  getSoleCandidates' constrains = (map ( \ (n, i:_) -> (n,i) )) . filter ((==1) .length . snd) $ constrains

derivePuzzles :: SudokuPuzzle -> (Int, [Int]) -> [SudokuPuzzle]
derivePuzzles puzzle constrain = ((map (flip updatePuzzle puzzle)) . (map (\ i -> [((fst constrain), i)]))) (snd constrain)

getCells :: SudokuPuzzle -> [Int]
getCells (GenPuzzle _ (GenBoard board)) = board

getPuzzleDimX :: SudokuPuzzle -> Int
getPuzzleDimY :: SudokuPuzzle -> Int
getPuzzleSize :: SudokuPuzzle -> Int
getPuzzleDimX (GenPuzzle (x, _) _) = x
getPuzzleDimY (GenPuzzle (_, y) _) = y
getPuzzleSize (GenPuzzle (x, y) _) = x * y

getPuzzleRowIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleRowIndicies   puzzle              rowID  = (map (\colID -> rowID * (getPuzzleSize puzzle) + colID)) [0..(getPuzzleSize puzzle)-1]
getPuzzleColIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleColIndicies   puzzle              colID  = (map (\rowID -> rowID * (getPuzzleSize puzzle) + colID)) [0..(getPuzzleSize puzzle)-1]
getPuzzleRectIndicies :: SudokuPuzzle -> Int -> [Int]
getPuzzleRectIndicies (GenPuzzle (x, y) _) rectID = (map (\rectElement -> (getIndexOfElementInRectangle x y rectID rectElement))) [0..x*y-1]
getPuzzleRow          :: SudokuPuzzle -> Int -> [Int]
getPuzzleRow  puzzle rowID  = (map ((getCells puzzle) !!)) (getPuzzleRowIndicies  puzzle rowID)
getPuzzleCol          :: SudokuPuzzle -> Int -> [Int]
getPuzzleCol  puzzle colID  = (map ((getCells puzzle) !!)) (getPuzzleColIndicies  puzzle colID)
getPuzzleRect         :: SudokuPuzzle -> Int -> [Int]
getPuzzleRect puzzle rectID = (map ((getCells puzzle) !!)) (getPuzzleRectIndicies puzzle rectID)

getIndexOfElementInRectangle :: Int -> Int -> Int -> Int -> Int
getIndexOfElementInRectangle x y rectNum rectElement = rowNum * size + colNum
  where
    size   = x * y
    rowNum = rectNum `quot` y * y + rectElement `quot` x
    colNum = rectNum `rem`  y * x + rectElement `rem`  x

buildPuzzleFromFormatedString :: String -> String -> SudokuPuzzle
buildPuzzleFromFormatedString dimensions square = GenPuzzle (x, y) board
  where
    [x, y] = map read . words $ dimensions
    board = (GenBoard . map readCell . words) square

buildFormatedStringFromPuzzle :: SudokuPuzzle -> [String] -> String
buildFormatedStringFromPuzzle puzzle _ = dimensions ++ "\n" ++ square
  where
    x = getPuzzleDimX puzzle
    y = getPuzzleDimY puzzle
    size = x * y
    dimensions = show x ++ " " ++ show y
    square = (unlines . map ((intercalate " ") . (map intToString) . (getPuzzleRow puzzle))) [0 .. size - 1]

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

-- Sole Candidate Strategy - values that are not persent in the same row/col/rect
-- returns a list of possible values for specific cell, so each of those values do not contradict with any of know values
getCandidate :: SudokuPuzzle -> Int -> Candidate
getCandidate puzzle elementNumber
  | getCells puzzle !! elementNumber > 0 = (elementNumber, [])
  | otherwise                                              = (elementNumber, [1..(getPuzzleSize puzzle)] \\ presentValues)
  where
    presentValues = [] `union` set1 `union` set2 `union` set3
    set1 = getPuzzleRow  puzzle (cellIDToRowID elementNumber puzzle)
    set2 = getPuzzleCol  puzzle (cellIDToColID elementNumber puzzle)
    set3 = getPuzzleRect puzzle (cellIDToRectID elementNumber puzzle)

-- Get candidates for all cells that are not filled yet
getCandidates :: SudokuPuzzle -> [Candidate]
getCandidates puzzle = map (getCandidate puzzle) candidateCellIds
  where
    size = getPuzzleSize puzzle
    cells = getCells puzzle
    candidateCellIds = filter (\val -> (cells !! val) == 0) cellIDs
    cellIDs = [0..size*size - 1]


-- for each row/col/rect checks whether a specific value can be assigned only to one cell in a unit
getLevelTwoUnitConstrains :: SudokuPuzzle -> [(Int, Int)]
getLevelTwoUnitConstrains puzzle = sort ([] `union` (concat . (map myVal)) [1..size])
  where
    constr indexFunction testVal = filter (\(_,values) -> testVal `elem` values) . map (getCandidate puzzle) . filterUnknows . indexFunction puzzle
    filterUnknows :: [Int] -> [Int]
    filterUnknows = filter (\val -> ((getCells puzzle) !! val) == 0)
    fullConstr :: Int -> [[(Int, [Int])]]
    fullConstr testVal = concat [map (constr getPuzzleRowIndicies testVal) [0..size - 1], map (constr getPuzzleColIndicies testVal) [0..size - 1], map (constr getPuzzleRectIndicies testVal) [0..size - 1]]
    filterUnique = filter (\val -> length(val) == 1)
    myVal :: Int -> [(Int, Int)]
    myVal testVal = ( (map (\ x -> ((fst . head) x, testVal)) ). filterUnique) (fullConstr testVal)
    size = getPuzzleSize puzzle

updatePuzzle :: [(Int, Int)] -> Update
updatePuzzle theUpdates = foldl1 (.) $ map updateWith theUpdates where
  updateWith (pos, val) (GenPuzzle size (GenBoard oldBoard)) = GenPuzzle size (GenBoard newBoard) where
    newBoard = left ++ (val:right)
    (left, (_:right)) = splitAt pos oldBoard

readCell :: String -> Int
readCell s
  | s == "_"    = 0
  | otherwise   = read s :: Int

intToString :: Int -> String
intToString s
  | s == 0    = "_"
  | otherwise   = show s
