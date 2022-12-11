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

parseMonkey :: [String] -> ([Integer], M)
parseMonkey (process -> 
  [
    ints -> [m],
    ints -> stack,
    drop 3 . words -> op,
    ints -> [d],
    ints -> [t],
    ints -> [f]
  ]) = 
    (fmap fromIntegral stack, 
     M m (parseOp op) (fromIntegral d) (t,f))

parse inp = (monkeys,state)
  where
    (stacks,monkeys) = unzip . fmap parseMonkey . paragraphs . lines $ inp
    ms = fmap m monkeys
    state = Map.fromList $ zip ms stacks
    mon = (monkeys !!)


step :: [M] -> (Integer -> Integer) -> S -> State C S
step monkeys post stacks = foldM go stacks monkeys
  where 
    check :: M -> Integer -> (Int,Integer)
    check (M m op d (t,f)) (post . op -> x) =
      (if x `mod` d == 0 then t else f, x)

    go :: S -> M -> State C S
    go stk mon =
      do 
        modify $ Map.adjust (+ genericLength s') m
        pure $ Map.insert m [] $ Map.unionWith (++) stk stk'
      where 
        (M m _ _ _) = mon
        s' = second (:[]) . check mon <$> (stk ! m)
        stk' = Map.fromListWith (flip (++)) s'

nest :: Monad m => Int -> (a -> m a) -> a -> m a
nest n f x = foldM (const . f) x (replicate n ())

go :: Int -> (Integer -> Integer) -> ([M],S) -> Integer
go n post (monkeys,state) = x*y
  where 
    ms = fmap m monkeys
    s0 = Map.fromList $ (id &&& const 0) <$> ms
    counts = execState (nest n (step monkeys post) state) s0
    [x,y] = take 2 $ reverse $ sort $ Map.elems counts

one = go 20 (`div` 3)

two x@(monkeys,state) = go 10000 (`mod` d') x
  where 
    d' = product $ fmap d monkeys


main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
