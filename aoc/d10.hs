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

data Op = Addx Integer | Noop deriving (Eq, Show, Read)
type S = Integer --deriving (Eq, Show)

parse :: String -> [Op]
parse = concatMap delay . fmap (read . tweak toUpper) . lines

delay :: Op -> [Op]
delay Noop = [Noop]
delay (Addx n) = [Noop, Addx n]

step :: S -> Op -> S
step x Noop = x
step x (Addx n) = x + n

one :: [Op] -> Integer
one ops = sum $ uncurry (*) <$> strength
  where 
    states = scanl step 1 ops
    during n = states `genericIndex` (n - 1)
    cycles = [20,60,100,140,180,220] :: [Integer]
    strength = (id &&& during) <$> cycles

two :: [Op] -> [String]
two ops = chunks 40 $ draw <$> zip pixels states
  where 
    draw (c,x) = if abs (c-x) <= 1 then '#' else '.'
    pixels = (`mod` 40) <$> [0..]
    states = scanl step 1 ops

main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ one inp
  traverse_ print $ two inp
