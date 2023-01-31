module FileTree (FileTree (..), prettyTree, readDirectory, flattenWith, flattenPaths, contentTree, getFileContents) where

-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html

import Control.Monad (forM, liftM, liftM2, mapM)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Printf (printf)
import Prelude hiding (readFile)

data FileTree = File {name :: FilePath} | Directory {name :: String, children :: [FileTree]} deriving (Show)

isDirectory :: FilePath -> IO Bool
isDirectory = doesDirectoryExist

getFileContents :: FilePath -> IO T.Text
getFileContents = readFile

withIsDir :: FilePath -> IO [(FilePath, Bool)]
withIsDir path = liftM2 zip files (files >>= traverse (isDirectory . subdir path))
  where
    files = listDirectory path

subdir :: FilePath -> FilePath -> FilePath
subdir = printf "%s/%s"

subTree :: FileTree -> FileTree -> FileTree
subTree parent child = case child of
  File path -> File newPath
  Directory path children -> Directory newPath children
  where
    newPath = subdir (name parent) (name child)

prettyTree :: FileTree -> String
prettyTree t = concat $ prettyTree' 0 [] t
  where
    tabs = concat . flip replicate "\t"
    prettyPath depth = printf "%s| %s\n" (tabs depth)
    prettyTree' :: Int -> [String] -> FileTree -> [String]
    prettyTree' depth acc tree = case tree of
      File name -> prettyPath depth name : acc
      Directory name children -> prettyPath depth name : concatMap (prettyTree' (depth + 1) acc) children

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

flattenWith :: (FilePath -> a) -> (FilePath -> [FileTree] -> a) -> FileTree -> [a]
flattenWith whenFile whenDir = inner []
  where
    inner acc tree = case tree of
      File path -> whenFile path : acc
      Directory path children -> whenDir path children : concatMap (inner acc . subTree tree) children

flattenPaths :: (FilePath -> a) -> FileTree -> [a]
flattenPaths f = flattenWith f (\p _ -> f p)

const2 :: a -> b -> c -> a
const2 = const . const

contentTree :: FileTree -> IO [(FilePath, T.Text)]
contentTree = mapM sequence . catMaybes . flattenWith (\p -> Just (p, getFileContents p)) (const2 Nothing)
