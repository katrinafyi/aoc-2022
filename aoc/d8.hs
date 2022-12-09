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


type Path = [String] --deriving (Eq, Ord, Show)

data File = File Int Path deriving (Eq, Ord, Show)
data Dir = Dir Int [Dir] [File] deriving (Eq, Ord, Show)

grid :: [[a]] -> [[((Int,Int),a)]]
grid arg = do
  (r,row) <- zip [0..] arg
  pure [((r,c),x) | (c,x) <- zip [0..] row]

type In = [[((Int, Int), Int)]]

parse :: String -> In
parse = grid . fmap (fmap (read . pure)) . lines

-- visible :: Ord a => [(a,Int)] -> Set a
visible = go (-1)
  where
    go h [] = Set.empty
    go h (x:xs)
      | snd x > h = Set.insert (fst x) $ go (snd x) xs
      | otherwise = go h xs

one trees = --length $ Set.unions $ concat [right,left,down,up]
  (right)
  where
    right = map visible trees
    left = map (visible . reverse) trees
    down = map visible (transpose trees)
    up = map (visible . reverse) (transpose trees)

two inp' = score <$> trees
  where
    size = maximum $ (\((x,_),_) -> x) <$> concat inp

    inp = fmap (fmap snd) inp'

    right = inp
    left = fmap reverse inp
    down = transpose inp
    up = fmap reverse (transpose inp)

    make = id

    -- score :: (Int,Int) -> Int
    score (pos@(r,c),h) = (pos,(a,b,x,y))
      where 
        s = size
        sight h trees = length $ see ++ take 1 rest
          where (see,rest) = span (< h) trees
        a = abs $ (-) c $ maybe s snd $ Set.lookupGT pos (right !! r)
        b = abs $ (-) c $ maybe 0 snd $ Set.lookupLT pos (left !! r)
        x = abs $ (-) r $ maybe s fst $ Set.lookupGT pos (down !! c)
        y = abs $ (-) r $ maybe 0 fst $ Set.lookupLT pos (up !! c)



    -- score = 



    -- make :: [((Int,Int),Int)] -> Set (Int,Int)
    -- make = Set.fromList . fmap fst
    -- make = id

main :: IO ()
main = do
  d <- getContents
  let inp = parse d
  print $ inp
  print $ one inp
  -- print $ one inp
  print $ two inp