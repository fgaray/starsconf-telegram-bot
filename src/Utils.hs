module Utils where

import Data.Maybe (fromJust)
import Data.List (stripPrefix)
import Data.Char (toLower)


stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix = (\(c:cs) -> toLower c : cs) .  fromJust . stripPrefix prefix
