module Formatting where

import qualified Data.Text as T
import FileTree (getFileContents)
import Match (FileMatch (matchList, matchPath))
import Text.Printf (printf)

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

-- Colour the file name green. See https://ss64.com/nt/syntax-ansi.html
formatMatch :: FileMatch -> String
formatMatch m = printf "\ESC[32m%s:\ESC[0m\n%s" (matchPath m) (T.unlines . map fstOf3 $ matchList m)