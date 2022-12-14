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
import Data.Function
import Data.Foldable


import Control.Arrow
import Control.Monad.Trans.State.Strict

import Text.ParserCombinators.ReadP ()
import qualified Text.ParserCombinators.ReadP as P


type S = Map Int [Integer]
type C = Map Int Integer

process :: [String] -> [String]
process = fmap (filter (not . (`elem` ",:")))

data M = M { m :: Int, op :: (Integer -> Integer), d :: Integer, tf :: (Int,Int) }

instance Show M where 
  show (M m _ d tf) = "M " ++ show m

parseOp :: [String] -> Integer -> Integer
parseOp ["old", o, n] =
  case n of 
    "old" -> \x -> op o x x
    _ -> \x -> op o x (read n)
  where 
    op "+" = (+)
    op "*" = (*)

parseMonkey :: [String] -> (Int, [Integer], M)
parseMonkey (process -> 
  [
    ints -> [m],
    ints -> stack,
    drop 3 . words -> op,
    ints -> [d],
    ints -> [t],
    ints -> [f]
  ]) = 
    (m, fmap fromIntegral stack, 
     M m (parseOp op) (fromIntegral d) (t,f))

parse inp = (monkeys,state)
  where
    (ms,stacks,monkeys) = unzip3 . fmap parseMonkey . paragraphs . lines $ inp
    state = Map.fromList $ zip ms stacks

step :: (Integer -> Integer) -> S -> M -> S
step post stk mon =
  Map.insert m [] $ Map.unionWith (++) stk stk'
  where 
    (M m _ _ _) = mon

    check :: M -> Integer -> (Int,Integer)
    check (M m op d (t,f)) (post . op -> x) =
      (if x `mod` d == 0 then t else f, x)

    s' = second (:[]) . check mon <$> (stk ! m)
    stk' = Map.fromListWith (flip (++)) s'

solve n post (monkeys,s0) = x*y
  where 
    repMonkeys = init $ concat $ replicate n monkeys

    states = scanl (step post) s0 repMonkeys

    num = length monkeys
    get mon state = state ! m mon

    -- stack processed by each monkey at each step
    stacks = 
      zipWith ($) (cycle (fmap get monkeys)) states
    
    -- number of items for each monkey at each step,
    -- monkey-major order.
    byMonkey = transpose $ chunks num $ fmap length stacks

    counts = fmap sum byMonkey
    x:y:_ = reverse $ sort counts

one = solve 20 (`div` 3)

two x@(monkeys,state) = solve 10000 (`mod` d') x
  where 
    d' = product $ fmap d monkeys

main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- traverse print $ one inp
  print $ one inp
  print $ two inp
