import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Data.Functor
import Data.Foldable

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P


type Path = [String] --deriving (Eq, Ord, Show)

data File = File Int Path deriving (Eq, Ord, Show)
data Dir = Dir Int [Dir] [File] deriving (Eq, Ord, Show)

class Size a where 
  size :: a -> Int
instance Size File where 
  size (File n _) = n 
instance Size Dir where 
  size (Dir n _ _) = n

data Ls = Ls Path [Path] [File] deriving (Eq, Ord, Show)

parseCd p = do 
  string "cd "
  l <- line'
  pure $ case l of 
    "/" -> [] 
    ".." -> tail p 
    x -> x : p 

parseDir p = do 
  string "dir "
  l <- line'
  pure $ (l : p)

parseFile p = do 
  s <- uint
  char ' '
  l <- line'
  pure $ File s <$> tails p

parseLs p = do 
  string "ls\n"
  items <- many $ eitherA (parseDir p) (parseFile p)
  let (dirs,files) = partitionEithers items
  pure $ concat files 

parseCommands p = (eof $> []) +++ do 
  string "$ "
  cmd <- eitherA (parseCd p) (parseLs p)
  case cmd of 
    Left p -> parseCommands p 
    Right ls -> (ls ++) <$> parseCommands p 

parse = readp $ do 
  parseCommands []

makeDisk :: [File] -> Map.Map Path Int
makeDisk = Map.fromListWith (+) . fmap (\(File s p) -> (p, s))

one = sum . filter (<= 100000) . Map.elems

total = 70000000

two disk = filter (>= need) . sort . Map.elems $ disk
  where 
    free = total - (disk ! [])
    need = 30000000 - free


main :: IO () 
main = do 
  d <- getContents 
  let inp = parse d
  let disk = makeDisk inp
  print $ inp
  print $ disk
  print $ one disk
  print $ two disk