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

parse :: String -> Seq Int
parse = Seq.fromList . fmap read . lines

data S = Moved | Unmoved deriving (Eq, Show)

-- assume item to be moved is at head of xs
go :: Int -> Seq (S,Int) -> Seq (S,Int)
go n ((Moved,x):<|xs) = go n (xs|>(Moved,x))
go n ((Unmoved,x):<|xs)
  | n /= x = go n (xs|>(Unmoved,x))
  | otherwise = l >< ((Moved,x) <| r)
  where
    len = Seq.length xs
    d = n `mod` len
    d' = if n < 0 then (d) `mod` len else d
    (l,r) = Seq.splitAt d' xs

mix order inp = snd <$> foldl' (flip go) ((Unmoved,) <$> inp) order

answer moved = fmap sum $ sequence $ (\x -> moved Seq.!? ((zero+x) `mod` len)) <$> ns
  where 
    len = Seq.length moved
    ns = [1000,2000,3000]
    zero = fromJust $ Seq.elemIndexL 0 moved

one inp = answer $ mix inp inp

two inp = answer y
  where 
    l = Seq.length inp
    x = fmap (*811589153) inp
    y = iterate (mix x) x !! 10

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ length inp 
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp

