-- Copyright (c) 2016, Mykola Pershyn

module Sudoku
    ( doMyMagic
    , SudokuPuzzle(..)
    , SudokuBoard(..)
    , solve
    , test
    ) where

import Data.List
import System.Exit(exitFailure, exitSuccess)

data SudokuPuzzle = GenPuzzle (Int, Int) SudokuBoard deriving Eq
data SudokuBoard  = GenBoard [Int] deriving Eq

defaultFormat = [] :: [String]

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
  | isSolved   puzzle            = [puzzle] -- solves test1
  | isSolvable puzzle == False   = []       -- solves test3
  | (isLevelOneUpdatable puzzle) = (solve . (updatePuzzle puzzle)
    . filterTrivialConstrains
    . buildLevelOneConstrains) puzzle       -- solves test2
  | (isLevelTwoUpdatable puzzle) = (solve . (updatePuzzle puzzle)
    . getLevelTwoUnitConstrains) puzzle     -- still not enough for test4
  | otherwise                  = (concat . (map solve) . (derivePuzzles puzzle)) ((head . buildLevelOneConstrains) puzzle)  -- solves slowly(test4)
  --	| otherwise              = [puzzle]  -- for testing

isValid :: SudokuPuzzle -> Bool
isValid puzzle
  | ((any(==True)) . (map ( \i -> any(>0) ((getPuzzleRow  puzzle i) \\ [1..getPuzzleSize puzzle])))) [0..(getPuzzleSize puzzle) - 1] = False
  | ((any(==True)) . (map ( \i -> any(>0) ((getPuzzleCol  puzzle i) \\ [1..getPuzzleSize puzzle])))) [0..(getPuzzleSize puzzle) - 1] = False
  | ((any(==True)) . (map ( \i -> any(>0) ((getPuzzleRect puzzle i) \\ [1..getPuzzleSize puzzle])))) [0..(getPuzzleSize puzzle) - 1] = False
  | otherwise = True

isSolved :: SudokuPuzzle -> Bool
isSolved puzzle = buildLevelOneConstrains puzzle == []

isSolvable :: SudokuPuzzle -> Bool
isSolvable puzzle = filter (\val -> snd val == []) (buildLevelOneConstrains puzzle) == []

isLevelOneUpdatable :: SudokuPuzzle -> Bool
isLevelOneUpdatable puzzle = (length . filterTrivialConstrains . buildLevelOneConstrains) puzzle > 0

isLevelTwoUpdatable :: SudokuPuzzle -> Bool
isLevelTwoUpdatable puzzle = (length . getLevelTwoUnitConstrains) puzzle > 0

filterTrivialConstrains :: [(Int, [Int])] -> [(Int, Int)]
filterTrivialConstrains constrains = ((map ( \ (n, i:is) -> (n,i) )) . (filter (\val -> (length . snd) val == 1))) constrains

derivePuzzles :: SudokuPuzzle -> (Int, [Int]) -> [SudokuPuzzle]
derivePuzzles puzzle constrain = ((map (updatePuzzle puzzle)) . (map (\ i -> [((fst constrain), i)]))) (snd constrain)

getPuzzleBoard :: SudokuPuzzle -> SudokuBoard
getPuzzleBoard (GenPuzzle _ board) = board
getPuzzleBoardAsListOfInts :: SudokuPuzzle -> [Int]
getPuzzleBoardAsListOfInts (GenPuzzle _ (GenBoard board)) = board

getPuzzleDimX :: SudokuPuzzle -> Int
getPuzzleDimY :: SudokuPuzzle -> Int
getPuzzleSize :: SudokuPuzzle -> Int
getPuzzleDimX (GenPuzzle (x, y) _) = x
getPuzzleDimY (GenPuzzle (x, y) _) = y
getPuzzleSize (GenPuzzle (x, y) _) = x * y

getPuzzleRowIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleColIndicies  :: SudokuPuzzle -> Int -> [Int]
getPuzzleRectIndicies :: SudokuPuzzle -> Int -> [Int]
getPuzzleRow          :: SudokuPuzzle -> Int -> [Int]
getPuzzleCol          :: SudokuPuzzle -> Int -> [Int]
getPuzzleRect         :: SudokuPuzzle -> Int -> [Int]
getPuzzleRowIndicies   puzzle              rowNum  = (map (\colNum -> rowNum * (getPuzzleSize puzzle) + colNum)) [0..(getPuzzleSize puzzle)-1]
getPuzzleColIndicies   puzzle              colNum  = (map (\rowNum -> rowNum * (getPuzzleSize puzzle) + colNum)) [0..(getPuzzleSize puzzle)-1]
getPuzzleRectIndicies (GenPuzzle (x, y) _) rectNum = (map (\rectElement -> (getIndexOfElementInRectangle x y rectNum rectElement))) [0..x*y-1]
getPuzzleRow  puzzle rowNum  = (map ((getPuzzleBoardAsListOfInts puzzle) !!)) (getPuzzleRowIndicies  puzzle rowNum)
getPuzzleCol  puzzle colNum  = (map ((getPuzzleBoardAsListOfInts puzzle) !!)) (getPuzzleColIndicies  puzzle colNum)
getPuzzleRect puzzle rectNum = (map ((getPuzzleBoardAsListOfInts puzzle) !!)) (getPuzzleRectIndicies puzzle rectNum)

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
buildFormatedStringFromPuzzle puzzle format = dimensions ++ "\n" ++ square
  where
    x = getPuzzleDimX puzzle
    y = getPuzzleDimY puzzle
    size = x * y
    dimensions = show x ++ " " ++ show y
    square = (unlines . map ((intercalate " ") . (map intToString) . (getPuzzleRow puzzle))) [0 .. size - 1]


-- returns a list of possible values for specific cell, so each of those values do not contradict with any of know values
getLevelOneCellConstrain :: SudokuPuzzle -> Int -> (Int,[Int])
getLevelOneCellConstrain puzzle elementNumber
  | getPuzzleBoardAsListOfInts puzzle !! elementNumber > 0 = (elementNumber, [])
  | otherwise                                              = (elementNumber, [1..(getPuzzleSize puzzle)] \\ presentValues)
  where
    presentValues = [] `union` set1 `union` set2 `union` set3
    set1 = getPuzzleRow  puzzle elementRow
    set2 = getPuzzleCol  puzzle elementCol
    set3 = getPuzzleRect puzzle elementRect
    elementRow  = elementNumber `quot` getPuzzleSize puzzle
    elementCol  = elementNumber `rem` getPuzzleSize puzzle
    elementRect = elementRow `quot` getPuzzleDimY puzzle * getPuzzleDimY puzzle + elementCol `quot` getPuzzleDimX puzzle

-- Builds a set of constrains. Each constrain is represented by its cell index and possible values
buildLevelOneConstrains :: SudokuPuzzle -> [(Int, [Int])]
buildLevelOneConstrains puzzle = ((map (getLevelOneCellConstrain puzzle)) . (filter (\val -> ((getPuzzleBoardAsListOfInts puzzle) !! val) == 0))) [0..size*size - 1]
  where
    size = getPuzzleSize puzzle


-- for each row/col/rect checks whether a specific value can be assigned only to one cell in a unit
getLevelTwoUnitConstrains :: SudokuPuzzle -> [(Int, Int)]
getLevelTwoUnitConstrains puzzle = sort ([] `union` (concat . (map myVal)) [1..size])
  where
    constr indexFunction testVal = ( (filter(\(_,values) -> testVal `elem` values)) . (map (getLevelOneCellConstrain puzzle)) . filterUnknows . (indexFunction puzzle) )
    filterUnknows :: [Int] -> [Int]
    filterUnknows = filter (\val -> ((getPuzzleBoardAsListOfInts puzzle) !! val) == 0)
    fullConstr :: Int -> [[(Int, [Int])]]
    fullConstr testVal = concat [map (constr getPuzzleRowIndicies testVal) [0..size - 1], map (constr getPuzzleColIndicies testVal) [0..size - 1], map (constr getPuzzleRectIndicies testVal) [0..size - 1]]
    filterUnique = filter (\val -> length(val) == 1)
    myVal :: Int -> [(Int, Int)]
    myVal testVal = ( (map (\ x -> ((fst . head) x, testVal)) ). filterUnique) (fullConstr testVal)
    size = getPuzzleSize puzzle

updatePuzzle :: SudokuPuzzle -> [(Int, Int)] -> SudokuPuzzle
updatePuzzle oldPuzzle theUpdates = GenPuzzle (getPuzzleDimX oldPuzzle, getPuzzleDimY oldPuzzle) (GenBoard newBoard)
  where
    newBoard = merge (getPuzzleBoardAsListOfInts oldPuzzle) theUpdates 0
    size = getPuzzleSize oldPuzzle
    merge :: [Int] -> [(Int, Int)] -> Int -> [Int]
    merge oldlist [] _ = oldlist
    merge oldlist (update:updates) shift = concat [take ((fst update) - shift) oldlist, [snd update], merge (drop ((fst update) + 1 - shift) oldlist) updates ((fst update) + 1)]


readCell :: String -> Int
readCell s
  | s == "_"    = 0
  | otherwise   = read s :: Int

intToString :: Int -> String
intToString s
  | s == 0    = "_"
  | otherwise   = show s
