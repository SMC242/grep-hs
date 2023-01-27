module Main where

import FileTree (FileTree)
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
  print $ "This is a grep clone written in Haskell" -- grep (toRegex expr)