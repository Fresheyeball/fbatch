module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils
import System.Environment(getArgs)
import System.Directory(renameFile, getDirectoryContents)

pathsContaining paths prefix = filter (isInfixOf prefix) paths
pathsRejected   paths reject = map (replace reject "") paths

main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let filesToRename = pathsContaining files reject
  let filesRenamed  = pathsRejected filesToRename reject

  putStrLn $ (show filesToRename) ++ (show filesRenamed)

  --renameFile old new
  --putStrLn $ "File " ++ old ++ " was renamed to " ++ new