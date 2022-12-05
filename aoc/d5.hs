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

data Move = Move { count :: Int, from :: Int, to :: Int } deriving Show
data In = In { stacks :: Map Int [Char], moves ::  [Move] } deriving Show

parseStacks :: ReadP (Map Int [Char])
parseStacks = do 
  x <- endBy line (char '\n')
  string " 1   2   3   4   5   6   7   8   9 "
  skipSpaces

  let a = (reverse x)

  let i n = 4 * (n-1) + 1
  let stack n = reverse $ filter (/= ' ') $ (!! i n) <$> a 
  let assocs = (\x -> (x, stack x)) <$> [1..9]

  -- many (satisfy (const True))

  pure $ Map.fromList assocs

parseMove = do 
  -- l <- munch (/= '\n')
  [n,from,to] <- ints <$> line
  skipSpaces
  pure $ Move n from to

-- parse :: String -> In
parse = readp $ do
  stacks <- parseStacks
  moves <- many parseMove
  pure $ In stacks moves

move :: Map Int [Char] -> Move -> Map Int [Char]
move sta (Move 0 from to) = sta 
move sta (Move n from to) = move m'' (Move (n-1) from to)
  where 
    x = head $ sta ! from
    m' = Map.adjust tail from sta 
    m'' = Map.adjust (x:) to m'

move' :: Map Int [Char] -> Move -> Map Int [Char]
move' sta (Move n from to) = m''
  where 
    x = take n $ sta ! from
    m' = Map.adjust (drop n) from sta 
    m'' = Map.adjust (x ++) to m'

one (In stacks moves) = head <$> snd <$> Map.toList final 
  where 
    final = foldl move stacks moves

two (In stacks moves) = head <$> snd <$> Map.toList final 
  where 
    final = foldl move' stacks moves

main :: IO () 
main = do 
  d <- getContents 
  --  transpose (reverse $ lines d)
  let inp = parse d
  print $ inp
  print $ one inp 
  print $ two inp 