{-# LANGUAGE OverloadedRecordDot #-}

module Board where
import Data.List (sortBy, nubBy)
import Data.Bifunctor (Bifunctor(second))

{-
   Co-ordinate system:

   Top left is 0,0
   Numbers increase as they go down and right
   First number is x axis  (Left to Right)
   Second is y axis (Up to Down)
-}

data Direction = DirRight | DirDown

data BoardWord = BoardWord {
   starting_pos :: (Int, Int)
   , direction :: Direction
   , wordValue :: String
}

charAtPos :: BoardWord -> [(Char, (Int, Int))]
charAtPos bword = zip bword.wordValue positions
   where
      (sx, sy) = bword.starting_pos
      positions = case bword.direction of
         DirDown -> zip (repeat sx) [sy..]
         DirRight -> zip [sx..] (repeat sy)

boardOrdering :: (Char, (Int, Int)) -> (Char, (Int, Int)) -> Ordering
boardOrdering (_, (x1, y1)) (_, (x2, y2)) =
   if y1 /= y2
      then y1 `compare` y2
      else x1 `compare` x2

foo :: Ordering -> Bool
foo EQ = True
foo _ = False

-- Doesn't check if there are incorrect/impossible boardwords that overlap
printBoard :: [BoardWord] -> IO ()
printBoard bwords =
   let chars = nubBy f $ sortBy boardOrdering $ concatMap charAtPos bwords
   in printChars chars
   where
      f a b = foo $ boardOrdering a b

printChars :: [(Char, (Int, Int))] -> IO ()
printChars [] = pure ()
printChars ((c, (x,y)):cs) = do
   let currLine = (c, (x,y)) : takeWhile linefilter cs
       otherlines = dropWhile linefilter cs
       withDroppedLine = fmap (second fst) currLine
   print $ printLine withDroppedLine
   printChars otherlines
   where
      linefilter = (== y) . snd . snd


printLine :: [(Char, Int)] -> String
printLine [] = ""
printLine ((char,index):rest) =
   let spaces = replicate index ' '
       next = fmap (second (\i -> i-(index+1))) rest
   in spaces ++ [char] ++ printLine next
