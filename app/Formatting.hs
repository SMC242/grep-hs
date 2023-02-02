module Formatting where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BLU
import FileTree (getFileContents)
import Match (FileMatch (matchList, matchPath))
import Text.Printf (printf)

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

unlinesB :: [BLU.ByteString] -> BLU.ByteString
unlinesB = B.concat . map (`B.append` BLU.fromString "\n")

-- Colour the file name green. See https://ss64.com/nt/syntax-ansi.html
formatMatch :: FileMatch -> String
formatMatch m = printf "\ESC[32m%s:\ESC[0m\n%s" (matchPath m) (BLU.toString . unlinesB . map fstOf3 $ matchList m)