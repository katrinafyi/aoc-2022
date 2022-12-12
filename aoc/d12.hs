{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Monad.State.Strict

import Text.ParserCombinators.ReadP ()
import qualified Text.ParserCombinators.ReadP as P

import qualified Data.Graph.Inductive as G


data S = 
  S { queue :: Seq (Int,Int),
      dists :: Map (Int,Int) Int
    }
  deriving (Eq, Show)

type P = (Int,Int)
data In = In (G.Gr P Int) (G.NodeMap P) P P [P]

indices :: Int -> [(Int,Int)]
indices w = go 0 
  where 
    go n = [(n,i) | i <- [0..w-1]] ++ go (n+1)

next :: Map (Int,Int) Int -> (Int,Int) -> [(Int,Int)]
next m pos = 
  filter allow $ fmap (pos+) deltas
  where 
    h = m ! pos 
    h' p = Map.findWithDefault infinity p m
    deltas = [(0,1),(0,-1),(1,0),(-1,0)]

    allow p = h' p <= h + 1 -- && h' p /= ord 'a'

make :: Map (Int,Int) Int -> (G.NodeMap P,G.Gr P Int)
make map = snd $ G.run G.empty $ do
  let nodes = Map.keys map
  let edges x = (x,,1) <$> next map x
  G.insMapNodesM nodes
  G.insMapEdgesM $ concatMap edges nodes

parse inp = In g m start end starts
  where 
    (m,g) = make map 

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

    starts = Map.keys $ Map.filter (== ord 'a') map

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
    step map

one (In g m s e _) = G.spLength (node s) (node e) g
  where 
    node x = fst $ G.mkNode_ m x

two (In g m _ e starts) = sort $ mapMaybe go starts
  where 
    go s = one (In g m s e starts) 

main :: IO ()
main = do
  inp <- parse <$> getContents
  -- print $ inp
  print $ one inp
  print $ two inp
