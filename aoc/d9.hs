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

grid :: [[a]] -> [[((Int,Int),a)]]
grid arg = do
  (r,row) <- zip [0..] arg
  pure [((r,c),x) | (c,x) <- zip [0..] row]


data Dir = U | D | L | R deriving (Eq, Show)

parse = concatMap p . lines 
  where 
    letter "L" = L 
    letter "U" = U 
    letter "R" = R 
    letter "D" = D
    p l = replicate (read n) (letter d)
      where [d,n] = words l 

-- ROW COLUMN
type P = (Int,Int)
type Rope = [P]

(x,y) |+| (a,b) = (x+a,y+b)
(x,y) |-| (a,b) = (x-a,y-b)
norm (x,y) = abs x + abs y


delta :: Dir -> P
delta U = (-1,0)
delta D = (1,0)
delta L = (0,-1)
delta R = (0,1)

move :: Dir -> P -> P
move d = (|+| delta d)

clamp x = (-1) `max` (1 `min` x)

follow :: P -> P -> P
follow h t = 
  let (dr,dc) = h |-| t in
  if abs dr `max` abs dc <= 1 then 
    t
  else 
    t |+| (clamp dr, clamp dc)

step :: Dir -> State Rope P
step d = do 
  modify $ \(h:t) -> move d h : t
  modify $ scanl1 follow 
  last <$> get


state0 n = replicate n z
  where z = (0,0)

simulate n ds = evalState (traverse step ds) (state0 n)
one ds = length $ Set.fromList $ simulate 2 ds
two ds = length $ Set.fromList $ simulate 10 ds

main :: IO ()
main = do
  d <- getContents
  let inp = parse d
  print $ inp
  print $ one inp
  -- print $ one inp
  print $ two inp