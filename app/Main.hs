module Main where

import Control.Monad
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe
import FileTree (FileTree, contentTree, prettyTree, readDirectory)
import Formatting (Colour (Red), colourise, formatMatch)
import Match (FileMatch, RegexMatch, matchTree, matches, toRegex)
import Options.Applicative
import Text.Regex.PCRE (CompOption, Regex, compCaseless, compExtended, compMultiline)

{-
  Structure:
  - Validate input RegEx
  - Read file tree (IO)
  - Read each file (IO) then:
    - Check if file contents match expression
    - Return Maybe (match text, match offset)
  - Format list of matched files and matches within each file
-}

data Args = Sample
  { flags :: String,
    expr :: String,
    path :: String
  }

parser :: Parser Args
parser =
  Sample
    <$> strOption
      ( short 'f'
          <> long "flags"
          <> metavar "FLAGS"
          <> help "Flags for the RegEx engine"
          <> value ""
      )
    <*> argument str (metavar "REGEX")
    <*> argument str (metavar "PATH")

acceptedFlags :: M.Map Char CompOption
acceptedFlags =
  M.fromList
    [ ('m', compMultiline),
      ('i', compCaseless),
      ('e', compExtended)
    ]

convertFlags :: String -> [CompOption]
convertFlags = mapMaybe (`M.lookup` acceptedFlags)

main :: IO ()
main = grep =<< execParser opts
  where
    opts = info (parser <**> helper) (fullDesc <> progDesc "Test" <> header "idk")

noMatchesMsg :: String
noMatchesMsg = colourise Red "No matches found :("

grep :: Args -> IO ()
grep args =
  parseRegex
    (expr args)
    (convertFlags $ flags args)
    ( \expr -> do
        matches <- readDirectory (path args) >>= matchTree expr
        let output = if null matches then noMatchesMsg else unlines $ map formatMatch matches
        putStrLn output
    )

parseRegex :: String -> [CompOption] -> (Regex -> IO ()) -> IO ()
parseRegex expr options = forM_ (toRegex options expr)