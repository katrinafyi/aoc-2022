{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq ((:<|)), (|>))
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Functor
import Data.Function
import Data.Foldable


import Control.Arrow
import Control.Applicative
import Control.Monad.State.Strict

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

import qualified Data.Graph.Inductive as G

type P = (Int,Int)

parse = fillLines . concat . fmap (p2 . p1 . uints) . lines
  where 
    p1 = fmap take2 . chunks 2
    p2 = fmap take2 . sliding 2

fill :: (P,P) -> [P]
fill (from,to)
  | from == to = [to]
  | otherwise = from : fill (from+delta,to)
  where 
    delta = signum $ to - from

fillLines :: [(P,P)] -> Set P
fillLines = Set.fromList . concatMap fill

step :: Int -> P -> Set P -> Either (Set P) (Set P)
step bot pos@(x,y) walls
  | start `Set.member` walls = Left walls
  | y > bot = Left walls
  -- | y > bot = error $ "fall through floor " ++ show pos
  | clear d = step bot d walls 
  | clear dl = step bot dl walls 
  | clear dr = step bot dr walls 
  | otherwise = Right $ Set.insert pos walls
  where
    clear = not . (`Set.member` walls)
    d = pos + (0,1)
    dl = pos + (-1,1)
    dr = pos + (1,1)

start = (500,0)

one walls = either length undefined final - length walls0
  where
    bottom = maximum $ Set.map snd walls
    final = iterateM 100000 (step bottom start) walls
    walls0 = walls

two walls = one (Set.union floor walls)
  where 
    bottom = maximum $ Set.map snd walls
    width = bottom
    floor = Set.fromList $ (,bottom+2) <$> [500-width-10..500+width+10]

main :: IO ()
main = do
  inp <- parse <$> getContents
  -- print $ inp
  print $ one inp
  print $ two inp
