{-# LANGUAGE FlexibleContexts #-}

module PartialMatch where

import Prelude hiding (words)

import Data.List (sort, sortBy)
import Control.Monad.Writer (Writer, tell, runWriter, replicateM)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Except (MonadError (throwError), catchError)
import Data.Bifunctor (Bifunctor(first))
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

import WordMap
import Control.Concurrent.Async (async, waitAny)

{-
sharedPartialMatch
[Char] -> [Char] -> String -> m (Bool, Char)
sharedPartialMatch canUse useOneOf word =
-}

partialMatch :: [Char] -> String -> Writer [Char] Bool
partialMatch [] [] = pure True
partialMatch [] _word = pure False
partialMatch (l:ls) [] = tell [l] >> partialMatch ls ""
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

tryNextWord ::
   (MonadError String m) =>
   [String] ->
   [(String, [Char])]
   -> m [String]
tryNextWord _ [] = throwError "no matches available"
tryNextWord words ((nextWord, remaining):otherOptions) =
   if remaining == ""
      then pure [nextWord]
      else do
         catchError
            (do
               downstreamMatches <- pMatchRecurse remaining words
               pure $ nextWord : downstreamMatches
            ) (const $ tryNextWord words otherOptions)

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
   tryNextWord words sortedByLongest
   where
      -- puts highest value words first
      sortfn (word1, _) (word2, _) = wordScrabbleValue word2 `compare` wordScrabbleValue word1

runRecurse :: WordMap -> [Char] -> [String] -> Either String [[String]]
runRecurse wordmap letters words = f <$> pMatchRecurse letters words
   where f = fmap (\word -> HashMap.findWithDefault [word] word wordmap)

runTryNextWord :: WordMap -> [String] -> (String, [Char]) -> Either String [[String]]
runTryNextWord wordmap words option = f <$> tryNextWord words [option]
   where f = fmap (\word -> HashMap.findWithDefault [word] word wordmap)

runRecurseThreaded :: 
   WordMap ->
   TBMQueue (String, [Char]) ->
   [String] ->
   IO (Either String [[String]])
runRecurseThreaded wordmap q words = loop
   where 
      loop = do
         mnext <- atomically $ readTBMQueue q
         case mnext of
            Nothing -> pure $ Left "no matches available"
            Just next -> case runTryNextWord wordmap words next of
               Left _ -> loop 
               Right results -> pure $ Right results

sendWords :: [a] -> TBMQueue a -> IO ()
sendWords xs q = do
   () <- mapM_ (atomically . writeTBMQueue q) xs
   atomically $ closeTBMQueue q

pMatchThreaded :: WordMap -> [String] -> IO ()
pMatchThreaded wordmap words = do
   letters <- getLine
   let sortedLetters = sort letters
       fpMatches = filterPartialMatches sortedLetters words
       sortedByLongest = sortBy sortfn fpMatches
   q <- newTBMQueueIO 10000
   threads <- replicateM 4 $ async $ runRecurseThreaded wordmap q words
   _ <- async $ sendWords sortedByLongest q
   (_, res) <- waitAny threads
   print res
   where
      -- puts highest value words first
      sortfn (word1, _) (word2, _) = wordScrabbleValue word2 `compare` wordScrabbleValue word1

pMatch :: WordMap -> [String] -> IO ()
pMatch wordmap words = do
   letters <- getLine
   let sortedLetters = sort letters
       matches = runRecurse wordmap sortedLetters words
   print matches

--eevosfobhwyeekesnrtexi
