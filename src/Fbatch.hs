module Fbatch where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Console.ANSI
import System.Directory(renameFile, renameDirectory, getDirectoryContents, doesDirectoryExist)
import qualified Control.Monad.Parallel as P

type FileName = String

data Rename = RenameDirectory FileName FileName
            | RenameFile      FileName FileName

replaceItoken :: String -> String -> (FileName, Int) -> String
replaceItoken r r' (o, i) = let t = replace "#{i}" (show i) r'
                            in replace r t o 

getDeltas :: String -> String -> [FileName] -> [(FileName, FileName)]
getDeltas r r' ps = let o = filter (r `isInfixOf`) ps
                        d = map (replaceItoken r r') (zip o [0..])
                    in zip o d

getRename :: FilePath -> FilePath -> IO Rename
getRename x y = do
  d <- doesDirectoryExist x
  if d
    then return $ RenameDirectory x y
    else return $ RenameFile      x y

rename :: Rename -> IO()
rename (RenameDirectory x y) = renameDirectory x y
rename (RenameFile      x y) = renameFile      x y

putInColor :: String -> Color -> IO()
s `putInColor` c = do
  setSGR [SetColor Foreground Vivid c]
  putStr s
  setSGR []

printRename :: (FileName, FileName) -> IO()
printRename (o, d) = do
  putStr "renaming: "
  (o ++ "\t") `putInColor` Yellow
  putStr " -> "
  (d ++ "\n") `putInColor` Blue

renameFromCli :: IO()
renameFromCli = do
  [reject, replacement, dir] <- getArgs
  files                      <- getDirectoryContents dir

  let deltas = getDeltas reject replacement files

  _ <- P.mapM (\(o, d) -> rename =<< getRename (dir </> o) (dir </> d)) deltas
  mapM_ printRename deltas
  putStrLn $ (show . length $ deltas) ++ " <- files renamed"