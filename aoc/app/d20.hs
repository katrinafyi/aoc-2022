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

-- parse :: String -> Seq Int
parse = fmap read . lines

data S = Moved | Unmoved deriving (Eq, Show)

-- assume item to be moved is at head of xs
go :: Int -> Seq (Int,Int) -> Seq (Int,Int)
go n ((i,x):<|xs)
  | n /= i = go n (xs |> (i,x))
  | otherwise = l >< ((i,x) <| r)
  where
    len = Seq.length xs
    d = x `mod` len
    (l,r) = Seq.splitAt d xs

mix inp = foldl' (flip go) inp is
  where
    l = Seq.length inp
    is = Seq.fromList [0..l-1]

answer moved = fmap sum $ sequence $ (\x -> moved Seq.!? ((zero+x) `mod` len)) <$> ns
  where 
    len = Seq.length moved
    ns = [1000,2000,3000]
    zero = fromJust $ Seq.elemIndexL 0 moved

one inp = answer $ fmap snd $ mix $ Seq.fromList $ zip [0..] inp

two inp = answer y
  where 
    l = length inp
    x = fmap (*811589153) inp
    in2 = Seq.fromList $ zip [0..] x
    y = fmap snd $ iterate mix in2 !! 10

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ length inp 
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp

