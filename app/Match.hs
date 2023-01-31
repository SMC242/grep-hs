module Match (matchTree, Match, toRegex, fileMatches) where

import Data.Array (elems, (!))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import FileTree (FileTree, contentTree, flattenWith)
import Text.Regex.TDFA

type Match = (T.Text, MatchOffset, MatchLength)

-- NOTE: Using options: extended regex, case sensitive, multiline
toRegex :: String -> Regex
toRegex = makeRegex

toMatch :: [MatchText T.Text] -> [Match]
toMatch = map ((\(text, (offset, length)) -> (text, offset, length)) . (! 0))

fileMatches :: Regex -> T.Text -> Maybe [Match]
fileMatches expr fileContents = case matchAllText expr fileContents of
  [] -> Nothing
  matches -> Just (toMatch matches)

matchTree :: Regex -> FileTree -> IO [Match]
matchTree expr tree = do
  let fileContents = contentTree tree

  return undefined

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