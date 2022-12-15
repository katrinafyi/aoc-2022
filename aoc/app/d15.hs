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
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

import qualified Data.Graph.Inductive as G

type P = (Int,Int)

parse = fmap p . fmap ints . lines
  where p [x,y,a,b] = ((x,y),(a,b))

diamond :: Int -> P -> [P]
diamond d pos = do
  dx <- [-d..d]
  let d' = abs (abs dx - d)
  dy <- [-d'..d']
  pure $ pos + (dx,dy)

mark (sensor,beacon) = Set.fromList $ diamond d sensor
  where 
    d = manhattan (beacon - sensor)

markAll inp = Set.unions $ mark <$> inp

one inp = 
  length $ Set.filter ((== y) . snd) $
    marked Set.\\ beacons
  where 
    beacons = Set.fromList $ snd <$> inp
    marked = markAll inp

    maxY = maximum $ snd . snd <$> inp
    y = if maxY > 100 then 2000000 else 10


two = id

main :: IO ()
main = do
  inp <- parse <$> getContents
  print $ one inp
  print $ two inp
