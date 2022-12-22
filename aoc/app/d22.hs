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

type P = (Int,Int)
data D = U | D | L | R deriving (Eq, Show)
data M = Map { byRow :: Map Int (Map P Bool), byCol :: Map Int (Map P Bool), mmap :: Map P Bool } deriving (Eq, Show)

data Move = Move Int | MoveL | MoveR deriving (Eq, Show)

pmap map = Map (f fst) (f snd) positions
  where 
    m '#' = Just False 
    m '.' = Just True 
    m _ = Nothing
    positions = Map.fromList $ fmap (second fromJust) $ filter (isJust . snd) $ concat $ indexed2 $ fmap (fmap m) map 
    f g = Map.fromList $ (\p -> (p,Map.filterWithKey (\k v -> p == g k) positions)) <$> ps
      where 
        ps = g <$> Map.keys positions
    
pmoves = P.many (n <|> d)
  where
    n = Move <$> int
    d = do
      c <- P.satisfy (`elem` ("RL" :: String))
      pure $ if c == 'R' then MoveR else MoveL

parse inp = (pmap map,readp pmoves $ concat moves)
  where
    ls = lines inp
    (map, moves) = break isEmpty ls

move :: D -> P 
move U = (-1,0)
move D = (1,0)
move L = (0,-1)
move R = (0,1)

ror U = R
ror R = D 
ror D = L 
ror L = U 

rol = ror . ror . ror

opp :: M -> (P,D) -> P
opp (Map rows cols _) (x,R) = (minimum $ Map.keys $ rows ! fst x)
opp (Map rows cols _) (x,L) = (maximum $ Map.keys $ rows ! fst x)
opp (Map rows cols _) (x,D) = (minimum $ Map.keys $ cols ! snd x)
opp (Map rows cols _) (x,U) = (maximum $ Map.keys $ cols ! snd x)

g :: M -> P -> (P,P)
g (Map rows cols _) (r,c) = (r `divMod` s, c `divMod` s)
  where s = Map.size cols `div` 3

opp2 :: M -> (P,D) -> (P,P) -> (P,D)
opp2 (Map rows cols _) (x,R) ((0,i),_) = (maximum $ Map.keys $ rows ! (150 - i - 1),L)
opp2 (Map rows cols _) (x,R) ((1,i),_) = (maximum $ Map.keys $ cols ! (100 + i), U)
opp2 (Map rows cols _) (x,R) ((2,i),_) = (maximum $ Map.keys $ rows ! (50-i-1), L)
opp2 (Map rows cols _) (x,R) ((3,i),_) = (maximum $ Map.keys $ cols ! (50+i),U)

opp2 (Map rows cols _) (x,L) ((0,i),_) = (minimum $ Map.keys $ rows ! (150 - i - 1),R)
opp2 (Map rows cols _) (x,L) ((1,i),_) = (minimum $ Map.keys $ cols ! (i), D)
opp2 (Map rows cols _) (x,L) ((2,i),_) = (minimum $ Map.keys $ rows ! (50-i-1), R)
opp2 (Map rows cols _) (x,L) ((3,i),_) = (minimum $ Map.keys $ cols ! (50+i),D)

opp2 (Map rows cols _) (x,D) (_,(0,i)) = (minimum $ Map.keys $ cols ! (100+i),D)
opp2 (Map rows cols _) (x,D) (_,(1,i)) = (maximum $ Map.keys $ rows ! (150+i), L)
opp2 (Map rows cols _) (x,D) (_,(2,i)) = (maximum $ Map.keys $ rows ! (50+i), L)

opp2 (Map rows cols _) (x,U) (_,(0,i)) = (minimum $ Map.keys $ rows ! (50+i),R)
opp2 (Map rows cols _) (x,U) (_,(1,i)) = (minimum $ Map.keys $ rows ! (150+i), R)
opp2 (Map rows cols _) (x,U) (_,(2,i)) = (maximum $ Map.keys $ cols ! (i), U)

go :: M -> Move -> (P,D) -> (P,D)
go _ MoveL (x,d) = (x,rol d)
go _ MoveR (x,d) = (x,ror d)
go _ (Move 0) x = x
go m@(Map rows cols map) (Move n) (x,d) =
  case next of 
    Nothing -> error "assertion fail, going to invalid space"
    Just False -> go m (Move 0) (x,d)
    Just True -> go m (Move (n-1)) (x',d')
  where
    step = x + move d 
    (x',d')
      | step `Map.member` map = (step,d)
      | otherwise = let aa = opp2 m (x,d) (g m x) in if (fst aa) `Map.notMember` map then undefined else aa
      
    next = map Map.!? x'


answer ((r,c),d) = (r+1) * 1000 + (c+1)*4 + f d
  where 
    f R = 0 
    f D = 1 
    f L = 2 
    f U = 3

one (m,moves) = answer $ foldl' (flip $ go m) (minimum $ Map.keysSet (mmap m), R) moves

two = id


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  -- print $ Map.size $ byCol (fst inp)
  -- print inp
  print $ one inp
  -- print $ two inp

