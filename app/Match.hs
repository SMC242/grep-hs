module Match (matchTree, FileMatch (..), RegexMatch (..), toRegex, matches) where

import Data.Array (elems, (!))
import Data.Bifunctor (second)
import qualified Data.ByteString.UTF8 as B
import Data.Functor ((<&>))
import Data.Maybe
import FileTree (FileTree, contentTree, flattenWith)
import Text.Regex.PCRE

data FileMatch = FileMatch
  { matchPath :: FilePath,
    matchList :: [RegexMatch]
  }
  deriving (Show)

type RegexMatch = (B.ByteString, MatchOffset, MatchLength)

-- NOTE: Using options: extended regex, case sensitive, multiline
toRegex :: String -> Maybe Regex
toRegex = makeRegexM

flattenMatch :: [MatchText B.ByteString] -> [RegexMatch]
flattenMatch = map ((\(text, (offset, length)) -> (text, offset, length)) . (! 0))

matches :: Regex -> B.ByteString -> Maybe [RegexMatch]
matches expr fileContents = case matchAllText expr fileContents of
  [] -> Nothing
  matches -> Just (flattenMatch matches)

matchTree :: Regex -> FileTree -> IO [FileMatch]
matchTree expr tree = do
  contentTree tree >>= mapM (pure . second (matches expr)) <&> map (uncurry FileMatch) . filterMaybeTuple
  where
    filterMaybeTuple :: [(a, Maybe b)] -> [(a, b)]
    filterMaybeTuple [] = []
    filterMaybeTuple (x : xs) =
      let rs = filterMaybeTuple xs
       in case x of
            (r1, Just r2) -> (r1, r2) : rs
            (_, Nothing) -> rs