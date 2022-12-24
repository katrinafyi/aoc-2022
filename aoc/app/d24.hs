{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq ((:<|)), (|>), (<|), (><))
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

-- import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.Heap(Heap, MinPrioHeap)
import qualified Data.Heap as Heap
import Data.Graph.AStar

import qualified Data.HashSet as HSet
import Data.HashSet(HashSet)
import Data.Hashable
import Data.MemoTrie
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import System.Exit

import GHC.Generics (Generic)

type P = (Int,Int)

u = (-1,0)
d = -u
r = (0,1)
l = -r

data M = M { blizz :: Set (P,P), walls :: Set P, start :: P, end :: P } deriving (Eq, Show)
data S = S { person :: P, time :: Int } deriving (Eq, Show, Ord, Generic)
instance Hashable S

parseblizz (pos,'<') = Just (pos,l)
parseblizz (pos,'>') = Just (pos,r)
parseblizz (pos,'v') = Just (pos,d)
parseblizz (pos,'^') = Just (pos,u)
parseblizz _ = Nothing

parse inp =
  M
    (Set.fromList $ mapMaybe parseblizz grid)
    walls
    (rmin,1)
    (rmax,cmax-1)
  where
    (rmin,rmax) = minmax $ Set.map fst walls
    (cmin,cmax) = minmax $ Set.map snd walls
    walls = Set.fromList . fmap fst . filter ((== '#'). snd) $ grid
    grid = concat . indexed2 . lines $ inp

xmod (x,y) (a,b) = (x `mod` a, (y `mod` b))

moveblizz :: M -> M
moveblizz m = m { blizz = blizz' }
  where
    (rmin,rmax) = minmax $ Set.map fst (walls m)
    (cmin,cmax) = minmax $ Set.map snd (walls m)
    rows = rmax-rmin+1-2
    cols = cmax-cmin+1-2
    blizz' = Set.map move (blizz m)
    move (pos,d)
      | (pos + d) `Set.member` walls m = (((pos+d-1) `xmod` (rows,cols)) + 1,d)
      | otherwise = (pos+d,d)

-- one :: M -> Int 
go m s0 send = aStar succs dist heust goal s0
  where 
    (rmin,rmax) = minmax $ Set.map fst (walls m)
    (cmin,cmax) = minmax $ Set.map snd (walls m)
    rows = rmax-rmin+1-2
    cols = cmax-cmin+1-2
    cyclelen = lcm rows cols

    blizzes = fmap (Set.map fst . blizz) $ take cyclelen $ iterate moveblizz m
    blizzat t = blizzes !! (t `mod` cyclelen)

    succs :: S -> HashSet S
    succs s = HSet.fromList $ fmap s2 $ filter (not . blocked) $ (+ person s) <$> dirs
      where 
        s2 p = S p (time s + 1)
        blocked d 
          | d `Set.member` blizzat (1 + time s) = True 
          | d `Set.member` (walls m) = True 
          | fst d < 0 = True 
          | otherwise = False
        dirs = [u,d,l,r,0] 

    dist x y = 1
    heust pos = manhattan $ send - person pos
    goal = (== send) . person

one m = go m (S (start m) 0) (end m)

two m = t3
  where 
    end1 = last $ fromJust $ one m
    end2 = last $ fromJust $ go m end1 (start m)
    t3 = go m end2 (end m)


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ Map.size $ byCol (fst inp)
  -- print inp
  -- traverse_ print $ fmap blizz $ take 5 $ iterate moveblizz inp
  print $ one inp
  print $ two inp

