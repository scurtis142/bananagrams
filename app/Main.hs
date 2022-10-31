{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (words)

import Data.List (sort, sortBy)
import Data.Char (toLower)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Bifunctor (Bifunctor(first))
-- import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError (throwError), catchError)

type WordMap = HashMap.HashMap String [String]

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

wordFilter :: String -> Bool
wordFilter str = notElem '\'' str && length str > 1

readWords :: IO [String]
readWords = do
   file <- readFile "words"
   let words = lines file
   pure $ fmap toLower <$> filter wordFilter words

softMatch :: String -> String -> Bool
softMatch [] _ = True
softMatch _ [] = False
softMatch (x:xs) (y:ys) =
   if x == y
      then softMatch xs ys
      else softMatch (x:xs) ys

softMatches :: String -> [String] -> [String]
softMatches toFind = filter $ softMatch toFind

partialMatch :: [Char] -> String -> Writer [Char] Bool
partialMatch [] [] = pure True
partialMatch (l:ls) [] = tell [l] >> partialMatch ls ""
partialMatch [] _word = pure False
partialMatch (l:ls) (w:ws) = 
   if l == w
      then partialMatch ls ws
      else tell [l] >> partialMatch ls (w:ws)

partialMatches :: [Char] -> [String] -> [(Maybe String, [Char])]
partialMatches letters = fmap f
   where f word = first (addword word) $ runWriter $ partialMatch letters word
         addword word isMatch = if isMatch then Just word else Nothing

filterPartialMatches :: [Char] -> [String] -> [(String, [Char])]
filterPartialMatches letters words = do
   (mword, rest) <- partialMatches letters words
   case mword of 
      Just word -> pure (word, rest)
      Nothing -> []

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

foobar :: WordMap -> [Char] -> [String] -> Either String [[String]]
foobar wordmap letters words = f <$> pMatchRecurse letters words
   where f = fmap (\word -> HashMap.findWithDefault [word] word wordmap)

pMatchLoop :: WordMap -> [String] -> IO ()
pMatchLoop wordmap words = do
   letters <- getLine
   let sortedLetters = sort letters
       matches = foobar wordmap sortedLetters words
   print matches

pMatchRecurse ::
   (MonadError String m) =>
   [Char] ->
   [String] ->
   m [String]
pMatchRecurse letters words =
   let sortedLetters = sort letters
       fpMatches = filterPartialMatches sortedLetters words
       sortedByLongest = sortBy sortfn fpMatches
   in
   beepboop words sortedByLongest
   where
      -- puts longest words first
      sortfn (word1, _) (word2, _) = wordScrabbleValue word2 `compare` wordScrabbleValue word1


beepboop ::
   (MonadError String m) =>
   [String] -> [(String, [Char])] -> m [String]
beepboop _ [] = throwError "no matches available"
beepboop words ((nextWord, remaining):otherOptions) =
   if remaining == ""
      then pure [nextWord]
      else do
         catchError
            (do
               matchedWords <- pMatchRecurse remaining words
               pure $ nextWord : matchedWords
            ) (const $ beepboop words otherOptions)


insertAll :: [String] -> WordMap -> WordMap
insertAll xs wordmap = foldl (flip insertToMap) wordmap xs

insertToMap :: String -> WordMap -> WordMap
insertToMap x = HashMap.insertWith f (sort x) [x]
   where f new old = new ++ old

vowelsConsonants :: String -> (String, String)
vowelsConsonants = rev . foldl f ("", "")
   where f (v, c) char = if char `elem` "aeiou" then (char:v, c) else (v, char:c)
         rev (v, c) = (reverse v, reverse c)

main :: IO ()
main = do
   
   words <- readWords

   let sorted = Vec.toList $ Vec.uniq $ Vec.fromList $ sort $ fmap sort words
       wordmap = insertAll words HashMap.empty

   -- sMatchLoop wordmap "" sorted
   pMatchLoop wordmap sorted
