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

import Data.MemoTrie
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import System.Exit

parse inp = Set.fromList $ fmap fst $ filter ((== '#') . snd) $ concat $  indexed2 $ lines inp

u = (-1,0)
d = -u
r = (0,1)
l = -r
ul = u+l
ur = u+r
dl = d+l
dr = d+r

type P = (Int,Int)
elf :: Set P -> [[P]] -> P -> P
elf map [a,b,c,d2] x
  | all (`Set.notMember` map) $ fmap (x+) [ul, u, ur, l, r, dl, d, dr] = x
  | all (`Set.notMember` map) $ fmap (x+) a = x+head a
  | all (`Set.notMember` map) $ fmap (x+) b = x+head b
  | all (`Set.notMember` map) $ fmap (x+) c = x+head c
  | all (`Set.notMember` map) $ fmap (x+) d2 = x+head d2
  | otherwise = x

elves :: Set P -> [[P]] -> Set P
elves map order = next
  where
    -- key: next position, value: prev position
    proposed = Map.fromListWith Set.union $ (elf map order &&& Set.singleton) <$> Set.toList map
    next = Map.foldrWithKey (\next prev set ->
      if Set.size prev > 1 then prev `Set.union` set else Set.insert next set)
      Set.empty
      proposed


order0 = [[u,ul,ur], [d,dl,dr], [l,ul,dl], [r,ur,dr]]
rot (x:xs) = xs ++ [x]
ordercycle = cycle $ take 4 $ iterate rot order0

answer map = area - Set.size map
  where
    (rmin,rmax) = minmax $ Set.map fst map
    (cmin,cmax) = minmax $ Set.map snd map
    area = (rmax-rmin+1) * (cmax-cmin+1)

rounds inp = scanl elves inp ordercycle

one = answer . (!! 10) . rounds 

two inp = succ $ length $ takeWhile (uncurry (/=)) $ sliding2 (rounds inp)


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ Map.size $ byCol (fst inp)
  print inp
  print $ one inp
  print $ two inp

