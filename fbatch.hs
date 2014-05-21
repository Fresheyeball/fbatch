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

addBaseToPaths  :: String -> [String] -> [String]
addBaseToPaths b p = map (\x -> b ++ "/" ++ x) p

getDeltas       :: String -> String -> [String] -> [(String, String)]
getDeltas b r ps = let o = addBaseToPaths b $ pathsContaining r ps
                       d = addBaseToPaths b $ pathsRejected   r ps
                       in zip o d

renameFromPairs :: [(FilePath, FilePath)] -> IO()
renameFromPairs [] = return ()
renameFromPairs (x:xs) = do
  renameFile (fst x) (snd x)
  renameFromPairs xs

main :: IO()
main = do
  args <- getArgs
  let directory = head args
  let reject    = last args

  files <- getDirectoryContents directory
  let count     = show $ length $ pathsContaining reject files
  renameFromPairs $ getDeltas directory reject files
  putStrLn (count ++ " files renamed")