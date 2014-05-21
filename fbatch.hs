module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils
import System.Environment(getArgs)
import System.Directory(renameFile, getDirectoryContents)

pathsContaining :: String -> [String] -> [String]
pathsContaining r ps = filter (isInfixOf r) ps

pathsReject     :: String -> [String] -> [String]
pathsReject r ps = map (replace r "") ps

pathsRejected   :: String -> [String] -> [String]
pathsRejected r ps = pathsReject r $ pathsContaining r ps
--pathsRejected = pathsReject >> pathsContaining

addBaseToPaths  :: String -> [String] -> [String]
addBaseToPaths b p = map (\x -> b ++ "/" ++ x) p

getDeltas b r ps = let o = addBaseToPaths b $ pathsContaining r ps
                       d = addBaseToPaths b $ pathsRejected   r ps
                       in zip o d


main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let filesRenamed = getDeltas directory reject files
  putStrLn (show filesRenamed)

  --renameFile old new
  --putStrLn $ "File " ++ old ++ " was renamed to " ++ new