
module SoftMatch where

import Prelude hiding (words)

import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap

import WordMap

softMatch :: String -> String -> Bool
softMatch [] _ = True
softMatch _ [] = False
softMatch (x:xs) (y:ys) =
   if x == y
      then softMatch xs ys
      else softMatch (x:xs) ys

softMatches :: String -> [String] -> [String]
softMatches toFind = filter $ softMatch toFind

sMatchLoop :: WordMap -> String -> [String] -> IO ()
sMatchLoop wordmap letters words = do

   let sortedLetters = sort letters
       sMatches = softMatches sortedLetters words
       -- pMatches = filterPartialMatches sortedLetters words

   print $ sMatches >>= \word -> HashMap.findWithDefault [word] word wordmap
   -- print $ pMatches >>= \(word, rest) -> pure (HashMap.findWithDefault [word] word wordmap, rest)

   -- print any direct matches
   print $ HashMap.lookup sortedLetters wordmap
   
   next <- getLine 
   let nextLetters = next ++ letters
   print nextLetters
   sMatchLoop wordmap nextLetters sMatches
