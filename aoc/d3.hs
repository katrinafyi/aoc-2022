import AocLib

import Data.List
import Data.Foldable
import Data.Bifunctor

import Data.Char


type In = [String]

parse :: String -> In
parse = lines

prio :: Char -> Int 
prio x
  | isUpper x = 26 + prio (toLower x)
  | otherwise = ord x - ord 'a' + 1

one = sum . fmap prio . concatMap (nub . foldl1 intersect) . fmap spl
  where 
    spl x = [a,b] where (a,b) = splitAt (length x `div` 2) x

two = sum . fmap prio . concatMap (nub . foldl1 intersect) . chunks 3

main :: IO ()
main = do 
  inp <- parse <$> getContents 
  print $ inp
  print $ one inp 
  print $ two inp 
