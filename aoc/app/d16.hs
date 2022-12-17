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

data V = V { key :: String, flow :: Int, next :: [String] }
  deriving (Eq, Ord)

instance Show V where
  show (V k _ _) = "V " ++ show k

type G a b = (G.Gr a b, a -> G.Node)

parsel l = V k f n
  where
    k = l !! 1
    n = filter (/= ',') <$> drop 9 l
    f = head . ints $ l !! 4

parse = fmap (parsel . words) . lines

valves = unsafePerformIO $ parse <$> getContents
valvemap = Map.fromList $ (key &&& id) <$> valves

pregraph :: [V] -> (G.Gr String Int, String -> G.Node)
pregraph vs = runGraph $ do
  let edge v = (key v,,1) <$> next v
  G.insMapNodesM (fmap key vs)
  G.insMapEdgesM $ concatMap edge vs

-- v = (valvemap !)
start = pgnode $ "AA"
(pg, pgnode) = pregraph valves

distance :: String -> String -> Int
distance = memo2 $ \from to ->
  fromMaybe (error $ "no path >" ++ from ++ "< >" ++ to ++ "<") $
    G.spLength (pgnode from) (pgnode to) pg

nz = Map.filter ((/= 0) . flow) valvemap
nzvset = Set.fromList $ Map.elems nz
nzset = Set.map key nzvset

nzsubsets = Set.powerSet nzset

setflow :: Set V -> Int
setflow vs = sum $ flow <$> Set.toList vs

allflow :: Int
allflow = setflow nzvset

type N = (Int,String,Set String)

duration = 30

succs :: HasCallStack => N -> [(N,N,Int)]
succs (_,"",_) = []
succs n@(t1,v1,open) = this ++ terminal ++ mapMaybe make (Set.toList closed)
  where
    end = duration
    closed = nzset Set.\\ open

    cost = allflow - setflow (Set.map (nz !) open)

    make v
      | v == v1 = Nothing
      | t1+d <= end = Just (n,(t1+d,v,open), f)
      | otherwise = Nothing
      where
        f = d * cost
        d = 0

    this
      | t1 < end && Set.member v1 closed = [(n,(t1+1,v1,Set.insert v1 open),cost)]
      | otherwise = []

    terminal
      | t1 <= end = [(n,endnode,(end-t1)*cost)]
      | otherwise = []

allnodes =
  do
    t <- times
    current <- Set.toList nzset
    open <- Set.toList nzsubsets
    guard $ Set.size open <= t
    pure (t,current,open)
  where
    times = [0..30]

setflow' = sum . map flow . map (nz !) . Set.toList . thd3

relax :: N -> N -> Map N Int -> Map N Int
relax from to dists
  | d' < old = Map.insert to d' dists
  | otherwise = dists
  where
    dt = fst3 to - fst3 from
    cost = dt * (allflow - setflow' from)
    d' = d + cost

    d = dists ! from
    old = fromMaybe maxBound $ dists Map.!? to



snd3 (x,y,z) = y
fst3 (x,y,z) = x
thd3 (x,y,z) = z

startnode = (0,"AA",Set.empty)
endnode = (duration,"",Set.empty)

edges = concatMap (succs) (startnode:allnodes)

g :: G.Gr N Int
gnode :: N -> G.Node
(g,gnode) = runGraph $ do
  G.insMapNodesM $ fmap fst3 edges ++ fmap snd3 edges
  G.insMapEdgesM $ edges


one = id

two = id


main :: HasCallStack => IO ()
main = do
  -- inp <- parse <$> getContents
  let inp = valves
  print inp
  -- G.prettyPrint $ fst $ pregraph inp
  -- print $ succs (0,"AA", Set.singleton ("AI") )
  print $ allflow
  print $ duration * allflow
  print $ succs endnode
  print $ length allnodes
  putStrLn $ "nodes done"
  print $ fmap (length . succs) allnodes
  print $ length edges
  putStrLn $ "edges done"
  print $ G.order g
  print $ G.size g
  let Just path = G.spLength (gnode startnode) (gnode endnode) g
  print $ path
  print $ duration * allflow - path
  -- print $ fmap (G.lab' . G.context g) path
  -- print $ one inp
  -- print $ two inp
