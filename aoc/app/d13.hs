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

data P = L [P] | I Int deriving (Eq, Show)

instance Ord P where 
  compare (I x) (I y) = compare x y
  compare (L x) (L y) = compare x y
  compare (L x) (I y) = compare x [I y]
  compare (I x) (L y) = compare [I x] y

parsePacket :: ReadP P 
parsePacket = do 
  let bracket = between (char '[') (char ']')
  let l = bracket (sepBy parsePacket (char ','))
  either L I <$> eitherA l uint

parse :: String -> [(P,P)]
parse = readp $ do 
  let p = take2 <$> P.count 2 (parsePacket <* skipSpaces)
  P.many p


test :: [(P,P)] -> [(Integer, Bool)]
test = fmap (second $ uncurry (<=)) . zip [1..]

one = sum . fmap fst . filter (uncurry (<=) . snd) . zip [1..]

flat = concatMap (\(x,y) -> [x,y])

two inp = liftA2 (*) (lookup d2 l) (lookup d6 l)
  where 
    sorted = sort . (++ [d2,d6]) . flat $ inp
    l = zip sorted [1..]

    d2 = L [L [I 2]]
    d6 = L [L [I 6]]

main :: IO ()
main = do
  inp <- parse <$> getContents
  -- print $ inp
  print $ one inp
  print $ two inp
  print $ test inp
