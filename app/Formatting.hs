module Formatting (colourise, Colour (..), formatMatch) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BLU
import qualified Data.Map as M
import FileTree (getFileContents)
import Match (FileMatch (matchList, matchPath))
import Text.Printf (printf)

data Colour = Red | Green | Yellow | Default deriving (Show, Eq, Ord)

-- See https://ss64.com/nt/syntax-ansi.html
colours :: M.Map Colour Int
colours =
  M.fromList
    [ (Red, 31),
      (Green, 32),
      (Yellow, 33),
      (Default, 0)
    ]

-- \ESC[0 at the end to return to default colours
colourise :: Colour -> String -> String
colourise c = printf "\ESC[%dm%s\ESC[0m" (colours M.! c)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

unlinesB :: [BLU.ByteString] -> BLU.ByteString
unlinesB = B.concat . map (`B.append` BLU.fromString "\n")

formatMatch :: FileMatch -> String
formatMatch m = template (matchPath m) (BLU.toString . unlinesB . map fst3 $ matchList m)
  where
    template :: String -> String -> String
    template path matches = colourise Green (path ++ ":\n") ++ matches