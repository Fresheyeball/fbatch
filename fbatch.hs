module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.Directory(renameFile, getDirectoryContents)

pathsContaining :: String -> [String] -> [String]
pathsContaining r ps = filter (isInfixOf r) ps

pathsReject     :: String -> [String] -> [String]
pathsReject r ps = map (replace r "") ps

pathsRejected   :: String -> [String] -> [String]
pathsRejected r = (pathsReject r) . (pathsContaining r)

addBaseToPaths  :: String -> [String] -> [String]
addBaseToPaths b p = map (\x -> b ++ "/" ++ x) p

getDeltas       :: String -> String -> [String] -> [(String, String)]
getDeltas b r ps = let o = addBaseToPaths b $ pathsContaining r ps
                       d = addBaseToPaths b $ pathsRejected   r ps
                       in zip o d

renameFromPairs :: [(FilePath, FilePath)] -> IO()
renameFromPairs [] = return ()
renameFromPairs (x:xs) = do
  (uncurry renameFile) x
  renameFromPairs xs

main :: IO()
main = do
  [directory, reject] <- getArgs
  files               <- getDirectoryContents directory
  let deltas = getDeltas directory reject files
  renameFromPairs deltas
  putStrLn $ (show . length $ deltas) ++ " files renamed"