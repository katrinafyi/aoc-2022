{-# LANGUAGE ViewPatterns #-}

import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Data.Functor
import Data.Foldable

import Control.Monad.State.Strict

import Text.ParserCombinators.ReadP ()
import qualified Text.ParserCombinators.ReadP as P

data Dir = U | D | L | R deriving (Eq, Show, Read)

parse :: String -> [Dir]
parse = concatMap p . lines 
  where 
    p (words -> [d,n]) = replicate (read n) (read d)

-- ROW COLUMN
type P = (Int,Int)
type Rope = [P]

delta :: Dir -> P
delta U = (-1,0)
delta D = (1,0)
delta L = (0,-1)
delta R = (0,1)

move :: Dir -> P -> P
move d = (+ delta d)

follow :: P -> P -> P
follow h t = 
  let (dr,dc) = h - t in
  if abs dr `max` abs dc <= 1 then 
    t
  else 
    t + signum (dr,dc)

step :: Rope -> Dir -> Rope
step rope d = scanl1 follow (tweak (move d) rope)

state0 n = replicate n (0,0)

simulate :: Int -> [Dir] -> [Rope]
simulate n = scanl step (state0 n)

visited :: [[P]] -> Set P
visited = Set.fromList . fmap last

one ds = length $ visited $ simulate 2 ds
two ds = length $ visited $ simulate 10 ds

main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ one inp
  print $ two inp
