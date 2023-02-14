module Match (matchTree, FileMatch (..), RegexMatch (..), toRegex, matches) where

import Data.Array (elems, (!))
import Data.Bifunctor (second)
import Data.Bits ((.|.))
import qualified Data.ByteString.UTF8 as B
import Data.Functor ((<&>))
import Data.Maybe
import FileTree (FileTree, contentTree)
import Text.Regex.PCRE

data FileMatch = FileMatch
  { matchPath :: FilePath,
    matchList :: [RegexMatch]
  }
  deriving (Show)

type RegexMatch = (B.ByteString, MatchOffset, MatchLength)

-- NOTE: `[CompOption]` must be summed to create a single `CompOption`
-- Source: guessed from source code
-- See: `wrapCompile` type signature in https://hackage.haskell.org/package/regex-pcre-0.95.0.0/docs/src/Text.Regex.PCRE.Wrap.html#CompOption
-- Bitwise OR used because the flags in the PCRE engine are represented as hexadecimal numbers
-- See: https://github.com/PCRE2Project/pcre2/blob/9c905ce0c19cd3ef94588c9d9f29b9b8a2457ecb/src/pcre2posix.h#L56-L68
sumOptions :: [CompOption] -> CompOption
sumOptions [] = defaultCompOpt
sumOptions xs = foldr (.|.) 0 xs

toRegex :: [CompOption] -> String -> Maybe Regex
toRegex options = makeRegexOptsM (sumOptions options) defaultExecOpt

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