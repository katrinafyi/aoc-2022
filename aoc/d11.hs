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

import Control.Arrow
import Control.Monad.State.Strict

import Text.ParserCombinators.ReadP ()
import qualified Text.ParserCombinators.ReadP as P


type S = Map Int [Int]

process :: [String] -> [String]
process = fmap (filter (not . (`elem` ",:")))

data M = M { m :: Int, op :: (Int -> Int), d :: Int, tf :: (Int,Int) }

instance Show M where 
  show (M m _ d tf) = "M " ++ show m

parseOp :: [String] -> Int -> Int
parseOp ["old", o, n] =
  case n of 
    "old" -> \x -> op o x x
    _ -> \x -> op o x (read n)
  where 
    op "+" = (+)
    op "*" = (*)

parseMonkey :: [String] -> ([Int], M)
parseMonkey (process -> 
  [
    ints -> [m],
    ints -> stack,
    drop 3 . words -> op,
    ints -> [d],
    ints -> [t],
    ints -> [f]
  ]) = (stack, M m (parseOp op) d (t,f))

parse inp = (monkeys,state)
  where
    (stacks,monkeys) = unzip . fmap parseMonkey . paragraphs . lines $ inp
    ms = fmap m monkeys
    state = Map.fromList $ zip ms stacks
    mon = (monkeys !!)


step :: [M] -> (Int -> Int) -> S -> S
step monkeys post stacks = foldl' go stacks monkeys
  where 
    check :: M -> Int -> (Int,Int)
    check (M m op d (t,f)) (post . op -> x) =
      (if x `mod` d == 0 then t else f, x)

    go :: S -> M -> S
    go stk mon =
      Map.insert m [] $ Map.unionWith (++) stk stk'
      where 
        (M m _ _ _) = mon
        s' = second (:[]) . check mon <$> (stk ! m)
        stk' = Map.fromListWith (flip (++)) s'

one (monkeys,state) = counts
  where 
    s = iterate (step monkeys (`div` 3)) state
    counts = Map.unionsWith (+) $ 
      Map.map length <$> take 21 s
    [x,y] = take 2 $ reverse $ sort $ Map.elems counts


two = id

main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  -- traverse_ print $ two inp
