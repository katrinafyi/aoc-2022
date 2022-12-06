module AocLib where

import Data.Char
import Data.List
import Data.Functor
import Data.Function
import Text.ParserCombinators.ReadP

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunks n zs

sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n (x:xs) = take n (x:xs) : sliding n xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = 
  case dropWhile p s of
    [] -> []
    s' -> w : splitBy p s''
          where (w, s'') = break p s'

split :: Eq a => a -> [a] -> [[a]]
split x = splitBy (== x)
  
take2 :: [a] -> (a,a)
take2 [x,y] = (x,y)
take2 _ = error "required two elements"

uint :: ReadP Integer 
uint = read <$> munch1 isDigit

int :: ReadP Integer 
int = do 
  sign <- option 1 (char '-' $> (-1))
  (* sign) <$> uint

ints :: String -> [Int]
ints = fmap read . filter (digit . head) . groupBy ((==) `on` digit)
  where 
    digit x = isDigit x || x `elem` "+-"

line :: ReadP String 
line = munch (/= '\n')


readp :: Show a => ReadP a -> String -> a 
readp p s = case readP_to_S (p <* eof) s of 
  [(x,[])] -> x
  [] -> error "readp error: no successful parse"
  x -> error $ "readp error: ambiguous parse " ++ show x