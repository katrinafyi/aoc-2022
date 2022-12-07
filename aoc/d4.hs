import AocLib

import Data.List
import Data.Maybe
import Data.Foldable

import Text.ParserCombinators.ReadP

type In = [[(Integer, Integer)]]

parseLine :: String -> [(Integer,Integer)]
parseLine = readp $ sepBy1 interval (char ',')
  where 
    interval = do 
      a <- uinteger 
      char '-'
      b <- uinteger 
      pure (a,b)


contains :: (Integer,Integer) -> (Integer,Integer) -> Bool 
contains (x,y) (a,b) = x <= a && b <= y

parse :: String -> In
parse = fmap parseLine . lines

one = length . filter go 
  where go [x,y] = x `contains` y || y `contains` x


overlap :: (Integer,Integer) -> (Integer,Integer) -> Integer
overlap (x,y) (a,b) = (min y b - max x a + 1) `max` 0

-- two :: In -> Int
two = length . filter (> 0) . fmap go 
  where go [x,y] = max (x `overlap` y) (y `overlap` x)


main :: IO () 
main = do 
  inp <- parse <$> getContents
  print $ one inp 
  print $ two inp 