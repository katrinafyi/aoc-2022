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
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

import qualified Data.Graph.Inductive as G
-- import qualified Data.Graph.Inductive.PatriciaTree as G

import Data.MemoTrie
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import System.Exit

parse = Set.fromList . fmap take3 . fmap uints . lines

type P = (Int,Int,Int)

shifts :: [P]
shifts = do
  [dx,dy,dz] <- replicateM 3 [-1..1]
  guard $ abs dx + abs dy + abs dz == 1
  pure (dx,dy,dz)

adjacents x = (x+) <$> shifts

exposed cubes x = exposed' ext x
  where ext = Set.fromList (adjacents x) Set.\\ cubes

exposed' ext x = length $ filter (`Set.member` ext) adj
  where adj = adjacents x

one cubes = sum $ exposed cubes <$> Set.toList cubes

lb = -1
ub = 25

bounded (x,y,z) = all (\x -> lb <= x && x <= ub) [x,y,z]

fill cubes start
  | start `Set.member` cubes = cubes
  | otherwise = foldl' fill cubes' adj
  where
    adj = filter bounded (adjacents start)
    cubes' = Set.insert start cubes

two cubes = sum $ exposed' air <$> Set.toList cubes
  where
    m = fromJust $ Set.lookupMax cubes
    outside = m + (1,0,0)
    filled = fill cubes outside
    air = filled Set.\\ cubes


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ one inp
  print $ two inp
  