module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Directory(renameFile, renameDirectory, getDirectoryContents, doesDirectoryExist)

getDeltas :: FilePath -> String -> [String] -> [(String, String)]
getDeltas r r' ps = let o = filter (r `isInfixOf`) ps
                        d = map (replace r r') o
                        in zip o d

rename :: FilePath -> FilePath -> IO()
rename x y = do   
  isDir <- doesDirectoryExist x
  if isDir
  then renameDirectory x y else renameFile x y

renameDeltaInBase :: FilePath -> (FilePath, FilePath) -> IO ()
renameDeltaInBase b (o,d) = do
  putStrLn ("renaming: " ++ o ++ " -> " ++ d)
  rename (b </> o) (b </> d)

main :: IO()
main = do
  [reject, replacement, directory] <- getArgs
  files <- getDirectoryContents directory
  let deltas = getDeltas reject replacement files
  mapM_ (renameDeltaInBase directory) deltas
  putStrLn $ (show . length $ deltas) ++ " files renamed"