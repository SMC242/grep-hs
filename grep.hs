-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

import Control.Monad (forM)
import Data.ByteString qualified as B
import Data.ByteString.UTF8 qualified as BLU
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, listDirectory) -- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html
import Text.Regex.TDFA

data FileTree = File {name :: String, contents :: BLU.ByteString} | Directory {name :: String, children :: [FileTree]} deriving (Show)

isDirectory :: FilePath -> IO Bool
isDirectory = doesDirectoryExist

getFileContents :: FilePath -> IO String
getFileContents path = undefined -- https://stackoverflow.com/questions/7867723/haskell-file-reading

-- Example way to test if files are directories
areDirs :: FilePath -> IO [Bool]
areDirs path = do
  files <- listDirectory path
  mapM isDirectory files -- TODO: convert to one-liner. https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad.html#v:liftM

walkFiles :: FilePath -> FileTree
walkFiles path = undefined

grep :: Regex -> FileTree -> [String]
grep expr fs = catMaybes $ inner [] fs
  where
    inner :: [Maybe String] -> FileTree -> [Maybe String]
    inner acc f = case f of
      File name contents -> (: acc) $ if matchTest expr contents then Just name else Nothing
      Directory name children -> concatMap (inner acc) children ++ acc

testTree :: FileTree
testTree =
  Directory
    "root"
    [ File "Data science" (BLU.fromString "Testing stuff on lots of data"),
      File "CBT" (BLU.fromString "Cognitive Behavioural Therapy"),
      Directory
        "Gonks"
        [ File "Original" (BLU.fromString "The first gonk; The progenitor"),
          File "My favourite" (BLU.fromString "Goth gonk <3")
        ],
      File "Top 10 Gaming moments" (BLU.fromString "10: Frotnite")
    ]

-- NOTE: Does not support flags
toRegex :: String -> Regex
toRegex = makeRegex

main :: IO ()
main = do
  expr <- getLine
  print $ grep (toRegex expr) testTree