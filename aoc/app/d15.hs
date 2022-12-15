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

border :: Int -> P -> [P]
border d pos = do
  dx <- [-d..d]
  let d' = abs (abs dx - d)
  dy <- nub [-d',d']
  pure $ pos + (dx,dy)

mark (sensor,beacon) = Set.fromList $ diamond d sensor
  where 
    d = manhattan (beacon - sensor)

markAll inp = Set.unions $ mark <$> inp

within :: Int -> P -> P -> Bool
within d x y = manhattan (x - y) <= d

one inp = length $ blocked Set.\\ beacons
  where 
    sensors = Set.fromList $ fst <$> inp
    radii = (\(s,b) -> (s,manhattan (s-b))) <$> inp
    beacons = Set.fromList $ snd <$> inp

    maxD = maximum $ manhattan . uncurry (-) <$> inp

    minX = minimum $ fst . fst <$> inp
    maxX = maximum $ fst . fst <$> inp

    y = if maxX > 1000 then 2000000 else 10

    row = (,y) <$> [minX-maxD..maxX+maxD]

    clear p = not $ any (\(s,r) -> within r s p) radii
    blocked = Set.fromList $ filter (not . clear) row


two inp = answer . nub $ filter clear search
  where 
    sensors = Set.fromList $ fst <$> inp
    radii = (\(s,b) -> (s,manhattan (s-b))) <$> inp

    minX = minimum $ fst . fst <$> inp
    maxX = maximum $ fst . fst <$> inp

    size = if maxX > 1000 then 4000000 else 20
    inbounds (x,y) = 
      0 <= min x y && max x y <= size
    search = filter inbounds $ 
      concatMap (\(s,r) -> border (r+1) s) radii

    clear p = not $ any (\(s,r) -> within r s p) radii
    -- blocked = Set.fromList $ filter (not . clear) row

    answer [(x,y)] = 
      fromIntegral x * 4000000 + fromIntegral y :: Integer

main :: IO ()
main = do
  inp <- parse <$> getContents
  -- print $ one inp
  print $ two inp
