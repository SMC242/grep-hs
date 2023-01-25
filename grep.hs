import Control.Monad (forM, liftM, liftM2, mapM)
-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html

import Data.Array ((!))
import Data.Encoding.UTF8
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO (readFile)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Regex.TDFA
import Prelude hiding (readFile)

{-
  Structure:
  - Validate input RegEx
  - Read file tree (IO)
  - Read each file (IO) then:
    - Check if file contents match expression
    - Return Maybe (match text, match offset)
  - Format list of matched files and matches within each file
-}

data FileTree = File {name :: FilePath} | Directory {name :: String, children :: [FileTree]} deriving (Show)

type Match = (T.Text, MatchOffset)

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

{-
TODO: qualifiy subdirs.
Something like f"{parent}/{child}""
See: https://stackoverflow.com/questions/1264797/string-interpolation-in-haskell
-}
toFileTree :: (FilePath, Bool) -> IO FileTree
toFileTree file = case file of
  (p, True) -> do
    tree <- walkFiles p
    pure $ Directory p tree
  (p, False) -> pure $ File p

-- walkFiles :: FilePath -> IO [IO FileTree]
walkFiles :: FilePath -> IO [FileTree]
walkFiles path = withIsDir path >>= mapM toFileTree

-- NOTE: Using options: extended regex, case sensitive, multiline
toRegex :: String -> Regex
toRegex = makeRegex

toMatch :: [MatchText T.Text] -> [Match]
toMatch = map ((\(text, (offset, _)) -> (text, offset)) . (! 0))

fileMatches :: Regex -> T.Text -> Maybe [Match]
fileMatches expr fileContents = case matchAllText expr fileContents of
  [] -> Nothing
  matches -> Just (toMatch matches)

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

-- matchTree :: Regex -> FileTree -> [String]
-- matchTree expr fs = catMaybes $ inner [] fs
--   where
--     inner :: [Maybe String] -> FileTree -> IO [Maybe String]
--     inner acc f = case f of
--       File name -> do
--         contents <- readFile name
--         pure . (: acc) $
--           if matchTest expr contents
--             then Just name
--             else Nothing
--       Directory name children -> pure $ concatMap (inner acc) children ++ acc

testTree :: FileTree
testTree =
  Directory
    "root"
    [ File "Data science",
      File "CBT",
      Directory
        "Gonks"
        [ File "Original",
          File "My favourite"
        ],
      File "Top 10 Gaming moments"
    ]

grep :: Regex -> FileTree -> [Match]
grep = undefined

main :: IO ()
main = do
  expr <- getLine
  print $ grep (toRegex expr) testTree