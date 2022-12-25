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

data S = N2 | N1 | Z | P1 | P2 deriving (Eq, Ord, Enum)
instance Show S where
  show = show . snafChr

snafNum N2 = -2
snafNum N1 = -1
snafNum Z = 0
snafNum P1 = 1
snafNum P2 = 2


snafChr N2 = '='
snafChr N1 = '-'
snafChr Z = '0'
snafChr P1 = '1'
snafChr P2 = '2'

numSnaf (-2) = N2
numSnaf (-1) = N1
numSnaf 0 = Z
numSnaf 1 = P1
numSnaf 2 = P2

parsesnafu '2' = Just P2
parsesnafu '1' = Just P1
parsesnafu '0' = Just Z
parsesnafu '-' = Just N1
parsesnafu '=' = Just N2
parsesnafu _ = Nothing

snafToInt :: [S] -> Integer
snafToInt = foldl' go 0
  where
    go :: Integer -> S -> Integer
    go x s = x*5 + snafNum s

go :: Integer -> [S]
go 0 = []
go n
  | r `elem` [0,1,2] = numSnaf r : go d
  | otherwise = numSnaf (r-5) : go (d+1)
  where
    (d,r) = n `divMod` 5

intToSnaf = reverse . go


parse = fmap (mapMaybe parsesnafu) . lines
one = fmap snafChr . intToSnaf . sum . fmap snafToInt
two = id

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ fmap snafToInt inp
  print $ fmap (snafToInt . intToSnaf . snafToInt) inp
  traverse_ print $ zip [0..] $   fmap (intToSnaf) [0..20]
  print $ one inp
  -- print $ two inp

