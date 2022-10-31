{-# LANGUAGE FlexibleContexts #-}

module PartialMatch where

import Prelude hiding (words)

import Data.List (sort, sortBy)
import Control.Monad.Writer (Writer, tell, runWriter)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Except (MonadError (throwError), catchError)
import Data.Bifunctor (Bifunctor(first))

import WordMap

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
