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
data S = S { h :: P, t :: P, seen :: Set P} deriving Show

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

follow :: State S () 
follow = do
  s@(S h t seen) <- get
  let (dr,dc) = h |-| t
  if abs dr `max` abs dc <= 1 then 
    pure () 
  else do
    let diff = (clamp dr, clamp dc)
    put $ s { t = t |+| diff}

see :: State S () 
see = do 
  modify $ \s -> s { seen = Set.insert (t s) (seen s) }

step :: Dir -> State S S
step d = do 
  s@(S h t seen) <- get
  put $ s { h = move d h}
  s@(S h t seen) <- get
  follow
  see
  get

state0 = S z z (Set.singleton z)
  where z = (0,0)

one ds = length seen
  where S _ _ seen = execState (traverse step ds) state0
two = const 0

main :: IO ()
main = do
  d <- getContents
  let inp = parse d
  print $ inp
  print $ one inp
  -- print $ one inp
  print $ two inp