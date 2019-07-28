module Main where

import System.IO
import Data.List
-- TODO: Client/Server
-- import Network.HTTP.Client.Request
-- TODO: use arrays instead
-- import Data.Array


import Sudoku

main :: IO ()
main = do
  --
  dimensions   <- getLine
  inputString  <- getContents
  let solution = ((intercalate ("There are " ++ ((show . length . (doMyMagic dimensions)) inputString) ++" solutions!\n")) . doMyMagic dimensions) inputString  -- do whatever magic needs to be done
  --	let solution = (((doMyMagic dimensions inputString)!!0) ++((show . length . (doMyMagic dimensions)) inputString) ++" solutions!\n")  -- just write the number of total solutions
  putStr solution -- write results
  return()
