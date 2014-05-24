module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Directory(renameFile, getDirectoryContents)

getDeltas :: String -> [String] -> [(String,String)]
getDeltas r ps = let o = filter (r `isInfixOf`) ps
                     d = map (replace r "") o
                    in zip o d

renameDeltaInBase :: String -> (String, String) -> IO ()
renameDeltaInBase b (o,d) = renameFile (b </> o) (b </> d)

main :: IO()
main = do
  [directory, reject] <- getArgs
  files               <- getDirectoryContents directory
  let deltas = getDeltas reject files
  mapM_ (renameDeltaInBase directory) deltas
  putStrLn $ (show . length $ deltas) ++ " files renamed"