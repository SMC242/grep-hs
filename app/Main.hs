module Main where

import FileTree (FileTree)
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