
module WordMap where

import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap

type WordMap = HashMap.HashMap String [String]

createWordMap :: [String] -> WordMap
createWordMap = foldl (flip insertToMap) HashMap.empty

insertToMap :: String -> WordMap -> WordMap
insertToMap x = HashMap.insertWith f (sort x) [x]
   where f new old = new ++ old

scabbleValue :: Char -> Int
scabbleValue char
   | char `elem` "aeioulnstr" = 1
   | char `elem` "dg" = 2
   | char `elem` "bcmp" = 3
   | char `elem` "fhvwy" = 4
   | char `elem` "k" = 5
   | char `elem` "jx" = 8
   | char `elem` "qz" = 10
   | otherwise = error "Found a non-lowercase letter"

wordScrabbleValue :: String -> Int
wordScrabbleValue = foldl f 0
   where f i =  (+) i . scabbleValue
