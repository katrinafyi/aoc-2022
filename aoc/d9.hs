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

delta :: Dir -> P
delta U = (-1,0)
delta D = (1,0)
delta L = (0,-1)
delta R = (0,1)

move :: Dir -> P -> P
move d = (|+| delta d)

follow :: P -> P -> P
follow h t = 
  let (dr,dc) = h |-| t in
  if abs dr `max` abs dc <= 1 then 
    t
  else 
    t |+| (signum dr, signum dc)

step :: Rope -> Dir -> Rope
step (h:t) d = do 
  scanl1 follow (move d h : t)

state0 n = replicate n z
  where z = (0,0)

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
