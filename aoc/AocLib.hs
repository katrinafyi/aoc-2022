module AocLib where

import Data.Char
import Data.List
import Data.Functor
import Data.Function
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.ReadP

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunks n zs

sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n (x:xs) = take n (x:xs) : sliding n xs

groupIf :: (a -> Bool) -> [a] -> [[a]]
groupIf f = go . dropWhile nf
  where 
    nf = not . f
    go [] = []
    go xs = yes : go (dropWhile nf no)
      where (yes,no) = span f xs

lstrip :: String -> String 
lstrip = dropWhile isSpace

isEmpty :: String -> Bool 
isEmpty = null . lstrip

paragraphs :: [String] -> [[String]]
paragraphs = groupIf (not . isEmpty)

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

tweak :: (a -> a) -> [a] -> [a]
tweak _ [] = [] 
tweak f (x:xs) = f x : xs

indexed :: (Num n, Enum n) => [a] -> [(n,a)]
indexed = zip [0..]

indexed2 :: [[a]] -> [[((Int,Int),a)]]
indexed2 = fmap go . indexed
  where
    go (r,row) = (\(c,x) -> ((r,c),x)) <$> indexed row

instance (Num a, Num b) => Num (a,b) where 
  (x,y) + (a,b) = (x+a,y+b)
  (x,y) - (a,b) = (x-a,y-b)
  (x,y) * (a,b) = (x*a,y*b)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger x = (fromInteger x,fromInteger x)

uinteger :: ReadP Integer 
uinteger = read <$> munch1 isDigit

integer :: ReadP Integer 
integer = do 
  sign <- option 1 (char '-' $> (-1))
  (* sign) <$> uinteger

uint :: ReadP Int
uint = read <$> munch1 isDigit

int :: ReadP Int
int = do 
  sign <- option id (char '-' $> negate)
  sign <$> uint

ints :: String -> [Int]
ints = fmap read . filter (digit . head) . groupBy ((==) `on` digit)
  where 
    digit x = isDigit x || x `elem` "+-"

line :: ReadP String 
line = munch (/= '\n')

line' = line <* char '\n'

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM n f x = foldM (&) x (replicate n f)

readp :: Show a => ReadP a -> String -> a 
readp p s = case readP_to_S (p <* eof) s of 
  [(x,[])] -> x
  [] -> error "readp error: no successful parse"
  x -> error $ "readp error: ambiguous parse " ++ show x