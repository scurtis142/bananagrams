{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (words)

import Data.List (sort)
import Data.Char (toLower)
import qualified Data.Vector as Vec
import WordMap
import PartialMatch

wordFilter :: String -> Bool
wordFilter str = notElem '\'' str && length str > 1

readWords :: IO [String]
readWords = do
   file <- readFile "words"
   let words = lines file
   pure $ fmap toLower <$> filter wordFilter words


main :: IO ()
main = do
   
   words <- readWords

   let sorted = Vec.toList $ Vec.uniq $ Vec.fromList $ sort $ fmap sort words
       wordmap = createWordMap words

   -- sMatchLoop wordmap "" sorted
   pMatch wordmap sorted
