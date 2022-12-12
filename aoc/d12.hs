{-# LANGUAGE ViewPatterns #-}

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
import Data.Either
import Data.Functor
import Data.Function
import Data.Foldable


import Control.Arrow
import Control.Monad.State.Strict

import Text.ParserCombinators.ReadP ()
import qualified Text.ParserCombinators.ReadP as P


data S = 
  S { queue :: Seq (Int,Int),
      dists :: Map (Int,Int) Int
    }
  deriving (Eq, Show)

data M = M { m :: Int, op :: (Integer -> Integer), d :: Integer, tf :: (Int,Int) }


indices :: Int -> [(Int,Int)]
indices w = go 0 
  where 
    go n = [(n,i) | i <- [0..w-1]] ++ go (n+1)

parse inp = (map,start,end)
  where 
    w = length $ takeWhile (not . isSpace) inp
    indexed = zip (indices w) $ filter (not . isSpace) inp

    find x = fst $ head $ filter ((== x) . snd) $ indexed
    start = find 'S'
    end = find 'E'

    h 'S' = 'a'
    h 'E' = 'z'
    h x = x

    heights = fmap (second h) indexed
    map = Map.fromList $ second ord <$> heights

next :: Map (Int,Int) Int -> (Int,Int) -> [(Int,Int)]
next m pos = 
  filter allow $ fmap (pos+) deltas
  where 
    h = m ! pos 
    h' p = Map.findWithDefault infinity p m
    deltas = [(0,1),(0,-1),(1,0),(-1,0)]

    allow p = h' p <= h + 1 -- && h' p /= ord 'a'

enqueue :: (Int,Int) -> State S () 
enqueue p = modify $ \s -> s { queue = queue s |> p}

infinity = maxBound `div` 2 :: Int

relax :: (Int,Int) -> Int -> State S ()
relax p d' = do 
  d <- gets (Map.findWithDefault infinity p . dists)
  if d <= d' then 
    pure ()
  else do 
    modify $ \s -> s { dists = Map.insert p d' (dists s) }
    enqueue p

step :: Map (Int,Int) Int 
  -> State S ()
step map = do 
  S queue dists <- get
  if null queue then 
    pure () 
  else do
    let (pos :<| queue') = queue
    let d = dists ! pos
    put $ S queue' dists

    let nexts = next map pos
    forM_ nexts (\p -> relax p (d+1))

one (inp,s,e) = Map.lookup e $ dists state
  where 
    -- adjust number of steps upwards until success
    state = execState (replicateM_ 100000 (step inp)) s0
    s0 = S (Seq.singleton s) (Map.singleton s 0)

two (inp,s,e) = sort $ mapMaybe (\s -> one (inp,s,e)) starts
  where 
    starts = Map.keys $ Map.filter (== ord 'a') inp

main :: IO ()
main = do
  inp <- parse <$> getContents
  -- print $ inp
  print $ one inp
  print $ two inp
