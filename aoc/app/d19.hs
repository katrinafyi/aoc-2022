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
import Data.Ratio

type P = (Int,Int,Int)
data BP = BP { orecost :: P, clacost :: P, obscost :: P, geocost :: P }
  deriving (Eq, Show)

parseb (ints->[i,a,b,c,d,e,f]) = BP (a,0,0) (b,0,0) (c,d,0) (e,0,f)
parse = fmap parseb . lines

infeasible = minBound `div` 2 :: Int

buy' :: P -> P -> Maybe (Int,P)
buy' cost@(a,b,c) income@(ore,cla,obs) =
  do
    ts <- zipWithM delay costs haves
    let t = maximum ts
    pure (t, income * fromIntegral t - cost)
  where
    costs = [a,b,c]
    haves = [ore,cla,obs]

    delay x 0 | x <= 0 = Just 0
    delay _ 0 = Nothing
    delay c dc = Just $ 0 `max` ceiling (c % dc)

-- cost, have, income
buy :: P -> P -> P -> Maybe (Int,P)
buy (a,b,c) (d,e,f) = buy' (a-d,b-e,c-f)

fst3 (x,y,z) = x
snd3 (x,y,z) = y
thd3 (x,y,z) = z

first3 f (x,y,z) = (f x,y,z)
second3 f (x,y,z) = (x,f y,z)
third3 f (x,y,z) = (x,y,f z)

gt :: P -> P -> Bool
gt (a,b,c) (d,e,f) = (a>=d) && (b>=e) && (c>=f)

geodes :: Int -> BP -> Int
geodes time (BP orecost clacost obscost geocost) =
  go time (0,0,0) (1,0,0)
  where
    costs = [orecost, clacost, obscost, geocost]
    maxore = maximum $ fmap fst3 costs
    maxcla = maximum $ fmap snd3 costs
    maxobs = maximum $ fmap thd3 costs

    go = memo go'

    go' :: Int -> P -> P -> Int
    go' t have@(ore,cla,obs) income@(dore,dcla,dobs)
      | t < 0 = undefined
      | otherwise = maximum (0:options1)
      where

        make :: (P -> P) -> Maybe (Int,P) -> Maybe Int
        make f (Just (dt,have2))
          | t-dt-1 >= 0 = Just $ go (t-dt-1) (have2+income) (f income)
        make f _ = Nothing

        make2 :: Maybe (Int,P) -> Maybe Int
        make2 (Just (dt,have2))
          | t-dt-1 >= 0 = Just $ (t-dt-1) + go (t-dt-1) (have2+income) income
        make2 _ = Nothing

        geobuy = buy geocost have income

        options1
          | income `gt` geocost = catMaybes [make2 geobuy]
          | otherwise = catMaybes
            [
              guard (dore < maxore) >> make (first3 succ) (buy orecost have income),
              guard (dcla < maxcla) >> make (second3 succ) (buy clacost have income),
              guard (dobs < maxobs) >> make (third3 succ) (buy obscost have income),
              make2 geobuy
            ]


answer = sum . zipWith (*) [1..]

one inp = fmap (geodes 24) inp
two inp = fmap (geodes 32) (take 3 inp)

bp = BP {orecost = (4,0,0), clacost = (4,0,0), obscost = (4,14,0), geocost = (2,0,16)}

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print bp 
  print inp
  -- print $ geodes bp
  print $ one inp
  let t = two inp
  print $ t 
  print $ product t
  