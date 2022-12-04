module AocLib where

import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunks n zs

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

readp :: ReadP a -> String -> a 
readp p s = case readP_to_S (p <* eof) s of 
  [(x,[])] -> x
  _:_ -> error "readp error: ambiguous parse"
  [] -> error "readp error: no successful parse"