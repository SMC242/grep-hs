module Match (matchTree, Match, toRegex, fileMatches) where

import Data.Array (elems, (!))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import FileTree (FileTree, contentTree, flattenWith)
import Text.Regex.TDFA

data FileMatch = Match
  { matchPath :: FilePath,
    matchContents :: T.Text,
    matchOffset :: MatchOffset,
    matchLength :: MatchLength
  }

type RegexMatch = (T.Text, MatchOffset, MatchLength)

-- NOTE: Using options: extended regex, case sensitive, multiline
toRegex :: String -> Regex
toRegex = makeRegex

flattenMatch :: [MatchText T.Text] -> [RegexMatch]
flattenMatch = map ((\(text, (offset, length)) -> (text, offset, length)) . (! 0))

fileMatches :: Regex -> T.Text -> Maybe [RegexMatch]
fileMatches expr fileContents = case matchAllText expr fileContents of
  [] -> Nothing
  matches -> Just (flattenMatch matches)

matchTree :: Regex -> FileTree -> [FileMatch]
matchTree expr tree = undefined

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