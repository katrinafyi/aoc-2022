module AocLib where

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