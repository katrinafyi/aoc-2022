import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

parse = head . lines

go n = (+ n) . length . fst . break mark . sliding n
  where 
    mark = (== n) . length . nub . sort 

one = go 4
two = go 14

main :: IO () 
main = do 
  d <- getContents 
  let inp = parse d
  print $ inp
  print $ one inp 
  print $ two inp 