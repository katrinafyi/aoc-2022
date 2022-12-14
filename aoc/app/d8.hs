import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Data.Functor
import Data.Foldable

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

type In = [[((Int, Int), Int)]]

parse :: String -> In
parse = indexed2 . fmap (fmap (read . pure)) . lines

-- visible :: Ord a => [(a,Int)] -> Set a
visible = go (-1)
  where
    go h [] = Set.empty
    go h (x:xs)
      | snd x > h = Set.insert (fst x) $ go (snd x) xs
      | otherwise = go h xs

one trees = length $ Set.unions $ concat [right,left,down,up]
  where
    right = map visible trees
    left = map (visible . reverse) trees
    down = map visible (transpose trees)
    up = map (visible . reverse) (transpose trees)

two inp = score <$> trees
  where
    size = maximum $ (\((x,_),_) -> x) <$> concat inp

    heights = fmap (fmap snd) inp
    trees = concat inp

    right = heights
    left = fmap reverse heights
    down = transpose heights
    up = fmap reverse (transpose heights)

    sight (h:trees) = see ++ take 1 rest
      where (see,rest) = span (< h) trees

    -- score :: (Int,Int) -> Int
    score (pos@(r,c),_) = product $ length <$> [a,b,x,y]
      where 
        a = sight $ drop c (right !! r)
        b = sight $ drop (size-c) (left !! r)
        x = sight $ drop r (down !! c)
        y = sight $ drop (size-r) (up !! c)

main :: IO ()
main = do
  d <- getContents
  let inp = parse d
  print $ inp
  print $ one inp
  -- print $ one inp
  print $ maximum $ two inp