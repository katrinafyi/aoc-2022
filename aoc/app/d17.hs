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

data D = L | R | D deriving (Eq, Show)

parse = mapMaybe p
  where
    p '<' = Just L
    p '>' = Just R
    p _ = Nothing

type P = (Int,Int)
type S = Set P

actions inp = concat $ transpose [cycle inp,repeat D]

move L = second (subtract 1)
move R = second (1+)
move D = first (subtract 1)

bounded :: Set P -> Bool
bounded x = all b x
  where
    b (y,x) = 0 <= x && x < 7

step :: Set P -> [D] -> Set P -> (Set P,[D])
step _ [] x = undefined
step shape (dir:rest) rocks
  | not (bounded shape') = step shape rest rocks
  | Set.disjoint shape' rocks =
    step shape' rest rocks
  | dir == D = (Set.union rocks shape,rest)
  | otherwise = step shape rest rocks
  where
    shape' = Set.map (move dir) shape

shapes = cycle $ Set.fromList . fmap swap <$>
  [
    fmap (,0) [0,1,2,3],
    fmap (,1) [0,1,2] ++ [(1,0), (1,2)],
    fmap (,0) [0,1] ++ fmap (2,) [0,1,2],
    fmap (0,) [0,1,2,3],
    fmap (,0) [0,1] ++ fmap (,1) [0,1]
  ]

rockfloor = Set.fromList $
  fmap (-1,) [0..6]

height = fst . Set.findMax

go :: [D] -> Int -> Set P -> [Set P]
go dirs n rocks = fmap fst $ scanl' f (rocks,dirs) shapes'
  where
    shapes' = take n shapes
    f :: (Set P,[D]) -> Set P -> (Set P,[D])
    f (rocks,dirs) = (\x -> step x dirs rocks) . Set.map ((+upper) *** (+2))
      where upper = 4 + height rocks

showgrid :: Set P -> String
showgrid x = unlines $ line <$> [h,h-1..0]
  where
    h = height x
    line h = [if p `Set.member` x then '#' else '.' | p <- pos]
      where
        pos = (h,) <$> [0..6]

topmost x = Set.filter ((>= 0) . fst) $ Set.map (first (subtract (h-5))) x
  where h = height x

match a b = topmost a == topmost b

one inp = (+1) $ height (last $ go inp 2022 rockfloor)

falldistance a b = height a - height (b Set.\\ a)

ignore x = pure ()

main :: HasCallStack => IO ()
main = do

  raw <- parse <$> getContents
  let inp = actions raw
  -- print inp

  let windlen = length raw * 2
  let shapelen = 4
  let l = lcm windlen shapelen

  -- offset <- (read <$> head <$> getArgs) :: IO Int
  print $ "multiple: " ++ show l

  let sample = go inp 4000 rockfloor
  let diff [a,b] = b-a 
  let ae = sortOn length . fmap (fmap fst) . groupBy ((==) `on` snd) . sortOn snd $ zip [0..] $ fmap topmost $ sample
  -- print ae
  -- print $ fmap (fmap diff . sliding 2) ae
  let c = 1745
  -- die "a"

  let m = ((`mod` l) *** (`mod` l))
  ignore $ sort $ id $ fmap (fst *** fst) $ filter (\((a,b),(c,d)) -> match b d) $ 
    take2 <$> sliding 2 (concat (transpose (chunks c
      (zip [0..] sample))))

  -- die "die offset finder"


  let offset = 220

  let mark0 = height $ last $ go inp offset rockfloor
  let mark = height $ last $ go inp (c+offset) rockfloor
  print $ "height per cycle: " ++ show (mark - mark0)

  let big = 1000000000000 :: Integer
  let (cycles,rest) = (big - fromIntegral offset) `divMod` fromIntegral c

  let mark1 = height $ last $ go inp (offset + fromIntegral rest) rockfloor
  print $ "rest height: " ++ show (mark1 - mark0)
  print $ "cycles: " ++ show cycles
  print $ "mark0 height " ++ show mark0
  print $ "p2 " ++ show (1 + fromIntegral mark1 + cycles * fromIntegral (mark - mark0))

  print $ showgrid $ topmost $ last $ go inp offset rockfloor
  -- print $ showgrid $ topmost $ last $ go inp (c+offset) rockfloor
  -- print $ showgrid $ topmost $ last $ go inp (c+offset+c) rockfloor
  -- print $ showgrid $ topmost $ last $ go inp (c+offset+c+c) rockfloor


  -- print $ one inp