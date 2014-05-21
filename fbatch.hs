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
pathsRejected = pathsReject >> pathsContaining

addBaseToPaths  :: String -> [String] -> [String]
addBaseToPaths b p = map (\x -> b ++ "/" ++ x) p

--getDeltas b r ps = do
--  let prefixBase xs = map (\x -> b ++ x) xs
--  let origin = prefixBase $ pathsContaining r ps
--  let deltas = prefixBase $ pathsRejected r ps
--  putStrLn $ show origin
--  return zip origin deltas



main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let filesRenamed = addBaseToPaths directory $ pathsRejected reject files

  putStrLn $ show $ pathsRejected reject files

  --renameFile old new
  --putStrLn $ "File " ++ old ++ " was renamed to " ++ new