{-# LANGUAGE LambdaCase #-}

module FileTree (FileTree (..), prettyTree, readDirectory, foldTree, flattenTree, flattenPaths, contentTree, getFileContents) where

-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html

import Control.Monad (forM, liftM, liftM2, mapM)
import qualified Data.ByteString.UTF8 as B
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Printf (printf)

data FileTree = File {name :: FilePath} | Directory {name :: String, children :: [FileTree]} deriving (Show)

isDirectory :: FilePath -> IO Bool
isDirectory = doesDirectoryExist

getFileContents :: FilePath -> IO B.ByteString
getFileContents path = B.fromString <$> readFile path

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

foldTree :: (FileTree -> a -> a) -> a -> FileTree -> [a]
foldTree f z t = case t of
  File path -> [x]
  Directory path children -> x : concatMap (foldTree f x . subTree t) children
  where
    x = f t z

prettyTree :: FileTree -> String
prettyTree t =
  concatMap snd $
    foldTree
      ( \t (depth, acc) -> case t of
          File path -> (depth, prettyPath depth path)
          Directory path _ -> (depth + 1, prettyPath depth path)
      )
      (0, "")
      t
  where
    tabs = concat . flip replicate "\t"
    prettyPath depth = printf "%s| %s\n" (tabs depth) . last . splitOn "/"  -- Remove parent paths

flattenTree :: (FileTree -> a) -> FileTree -> [a]
flattenTree f t = case t of
  File _ -> [f t]
  Directory _ children -> f t : concatMap (flattenTree f . subTree t) children

flattenPaths :: (FilePath -> a) -> FileTree -> [a]
flattenPaths f = flattenTree (f . name)

contentTree :: FileTree -> IO [(FilePath, B.ByteString)]
contentTree =
  mapM sequence
    . catMaybes
    . flattenTree
      ( \case
          File path -> Just (path, getFileContents path)
          Directory _ _ -> Nothing
      )
