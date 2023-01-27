module FileTree (FileTree, readDirectory) where

import Data.Text.IO (readFile)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Printf (printf)
import Prelude hiding (readFile)
import Data.Text qualified as T

data FileTree = File {name :: FilePath} | Directory {name :: String, children :: [FileTree]} deriving (Show)

isDirectory :: FilePath -> IO Bool
isDirectory = doesDirectoryExist

getFileContents :: FilePath -> IO T.Text
getFileContents = readFile

-- Example way to test if files are directories
areDirs :: FilePath -> IO [Bool]
areDirs path = listDirectory path >>= traverse isDirectory

withIsDir :: FilePath -> IO [(FilePath, Bool)]
withIsDir path = liftM2 zip files (files >>= traverse isDirectory)
  where
    files = listDirectory path

subdir :: String -> String -> String
subdir = printf "%s/%s"

prettyTree :: FileTree -> String
prettyTree t = concat $ prettyTree' 0 [] t
  where
    tabs = concat . flip replicate "\t"
    prettyPath depth = printf "%s| %s\n" (tabs depth)
    prettyTree' :: Int -> [String] -> FileTree -> [String]
    prettyTree' depth acc tree = case tree of
      File name -> prettyPath depth name : acc
      Directory name children -> prettyPath depth name : concatMap (prettyTree' (depth + 1) acc) children

{-
TODO: qualifiy subdirs.
Something like f"{parent}/{child}""
See: https://stackoverflow.com/questions/1264797/string-interpolation-in-haskell
-}
-- Use readDirectory instead
walkFiles :: FilePath -> IO [FileTree]
walkFiles path = withIsDir path >>= mapM toFileTree
  where
    toFileTree :: (FilePath, Bool) -> IO FileTree
    toFileTree file = case file of
      (p, True) -> do
        tree <- walkFiles (subdir path p)
        pure $ Directory p tree
      (p, False) -> pure $ File p

-- Use this function for getting FileTrees
readDirectory :: FilePath -> IO FileTree
readDirectory path = Directory path <$> walkFiles path

-- walkFiles :: FilePath -> IO [FileTree]
-- walkFiles path = forM withIsDir toFileTree
--   where
--     files = listDirectory path
--     withIsDir = liftM2 zip files (files >>= traverse isDirectory)
--     toFileTree :: (FilePath, Bool) -> IO FileTree
--     toFileTree (name, isDir) = do
--       subTree <- walkFiles name
--       contents <- getFileContents name
--       if isDir
--         then fmap Directory name subTree
--         else fmap File name contents