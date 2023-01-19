import Data.ByteString qualified as B
import Data.ByteString.UTF8 qualified as BLU -- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)

data FileTree = File {name :: String, contents :: BLU.ByteString} | Directory {name :: String, children :: [FileTree]} deriving (Show)

grep :: BLU.ByteString -> FileTree -> [String]
grep x fs = catMaybes $ inner [] fs
  where
    inner :: [Maybe String] -> FileTree -> [Maybe String]
    inner acc f = case f of
      File name contents -> (: acc) $ if x `B.isInfixOf` contents then Just name else Nothing
      Directory name children -> concatMap (inner acc) children ++ acc

testTree :: FileTree
testTree =
  Directory
    "root"
    [ File "Data science" (BLU.fromString "Testing stuff on lots of data"),
      File "CBT" (BLU.fromString "Cognitive Behavioural Therapy"),
      Directory
        "Gonks"
        [ File "Original" (BLU.fromString "The first gonk; The progenitor"),
          File "My favourite" (BLU.fromString "Goth gonk <3")
        ]
    ]

main :: IO ()
main = print $ grep (BLU.fromString "gonk") testTree