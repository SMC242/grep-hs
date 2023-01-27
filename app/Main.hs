module Main where

import FileTree (FileTree, prettyTree, readDirectory)
import Match (Match, toRegex)
import Text.Regex.TDFA (Regex)

{-
  Structure:
  - Validate input RegEx
  - Read file tree (IO)
  - Read each file (IO) then:
    - Check if file contents match expression
    - Return Maybe (match text, match offset)
  - Format list of matched files and matches within each file
-}

grep :: Regex -> FileTree -> [Match]
grep = undefined

main :: IO ()
main = do
  expr <- getLine
  s <- prettyTree <$> readDirectory "../test"
  putStr s