module Main where

import Control.Monad
import FileTree (FileTree, contentTree, prettyTree, readDirectory)
import Formatting (formatMatch)
import Match (FileMatch, RegexMatch, matchTree, matches, toRegex)
import Text.Regex.TDFA (Regex, RegexMaker (makeRegex))

{-
  Structure:
  - Validate input RegEx
  - Read file tree (IO)
  - Read each file (IO) then:
    - Check if file contents match expression
    - Return Maybe (match text, match offset)
  - Format list of matched files and matches within each file
-}

grep :: Regex -> FileTree -> [FileMatch]
grep = undefined

main :: IO ()
main = do
  let expr = makeRegex "Gonk"
  matches <- readDirectory "../test" >>= matchTree expr
  putStr . unlines $ map formatMatch matches