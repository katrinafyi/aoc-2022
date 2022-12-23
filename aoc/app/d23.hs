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

rounds :: Int -> [[P]] -> Set P -> Either (Int) (Set P)
-- rounds 0 _ x = undefined
rounds n order x
  | map' == x = Left n
  | otherwise = rounds (n-1) order' map'
  where 
    map' = elves x order
    order' = tail order ++ [head order]

order0 = [[u,ul,ur], [d,dl,dr], [l,ul,dl], [r,ur,dr]]

answer map = area - Set.size map
  where 
    rows = Set.map fst map 
    cols = Set.map snd map
    area = (maximum rows - minimum rows + 1) * (maximum cols - minimum cols + 1)

one inp = fromLeft undefined $ fixM (rounds (-1) order0) inp

two = id

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ Map.size $ byCol (fst inp)
  print inp
  print $ one inp
  -- print $ two inp

