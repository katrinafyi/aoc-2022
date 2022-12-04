import AocLib

import Data.List
import Data.Maybe
import Data.Foldable
import Data.Bifunctor

type In = [[(Int, Int)]]

parseLine :: String -> [(Int,Int)]
parseLine s = take2 <$> fmap read <$> split '-' <$> split ',' s

contains :: (Int,Int) -> (Int,Int) -> Bool 
contains (x,y) (a,b) = x <= a && b <= y

parse :: String -> In
parse = fmap parseLine . lines

one = length . filter go 
  where go [x,y] = x `contains` y || y `contains` x


overlap :: (Int,Int) -> (Int,Int) -> Int
overlap (x,y) (a,b) = (min y b - max x a + 1) `max` 0

-- two :: In -> Int
two = length . filter (> 0) . fmap go 
  where go [x,y] = max (x `overlap` y) (y `overlap` x)


main :: IO () 
main = do 
  inp <- parse <$> getContents
  print $ one inp 
  print $ two inp 