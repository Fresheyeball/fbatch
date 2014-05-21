module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils
import System.Environment(getArgs)
import System.Directory(renameFile, getDirectoryContents)

pathsContaining :: String -> [String] -> [String]
pathsContaining prefix paths = filter (isInfixOf prefix) paths

pathsRejected   :: String -> [String] -> [String]
pathsRejected   reject paths = map (replace reject "") paths

--pathsCR paths reject = 

main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let filesToRename = pathsContaining reject files 
  let filesRenamed  = pathsRejected reject filesToRename 

  putStrLn $ (show filesToRename) ++ (show filesRenamed)

  --renameFile old new
  --putStrLn $ "File " ++ old ++ " was renamed to " ++ new