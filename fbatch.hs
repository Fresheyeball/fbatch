module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils
import System.Environment(getArgs)
import System.Directory(renameFile, getDirectoryContents)

pathsContaining :: String -> [String] -> [String]
pathsContaining prefix paths = filter (isInfixOf prefix) paths

pathsReject     :: String -> [String] -> [String]
pathsReject     reject paths = map (replace reject "") paths

pathsRejected   :: String -> [String] -> [String]
pathsRejected   reject paths = pathsReject reject (pathsContaining reject paths)

main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let filesRenamed  = pathsRejected reject files

  putStrLn $ show filesRenamed

  --renameFile old new
  --putStrLn $ "File " ++ old ++ " was renamed to " ++ new