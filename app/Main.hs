module Main where

import Console.Options
import Control.Monad
import Data.Functor ((<&>))
import FileTree (FileTree, contentTree, prettyTree, readDirectory)
import Formatting (formatMatch)
import Match (FileMatch, RegexMatch, matchTree, matches, toRegex)
import Text.Regex.PCRE (Regex, RegexMaker (makeRegex, makeRegexM))

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
  inputRegex
    ( \expr -> do
        matches <- readDirectory "../test" >>= matchTree expr
        putStr . unlines $ map formatMatch matches
    )

readRegex :: String -> Maybe Regex
readRegex = makeRegexM

inputRegex :: (Regex -> IO ()) -> IO ()
inputRegex continue = do
  expr <- getLine
  forM_ (readRegex expr) continue

-- main' :: IO ()
-- main' = defaultMain $ do
--   programName "grep-hs"
--   programDescription "Clone of grep(1)"
--   flagIgnore <- flag $ FlagShort "i" <> FlagLong "ignore-case"
--   flagMultiline <- flag $ FlagShort "m" <> FlagLong "multiline"
--   rest <- remainingArguments "FILE"
--   action $ \toParam -> do
