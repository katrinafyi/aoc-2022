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


data Cmd = Cd Path | Ls Path [Path] [File] deriving (Eq, Ord, Show)

parseCd :: Path -> ReadP Cmd
parseCd p = do 
  string "cd "
  a <- line 
  char '\n'
  pure $ Cd $ case a of 
    "/" -> [] 
    ".." -> tail p 
    x -> x : p 

parseDir p = do 
  string "dir "
  l <- line 
  char '\n'
  pure $ (l : p)

parseFile p = do 
  s <- fromInteger <$> uint 
  char ' '
  l <- line
  char '\n'
  pure $ File s (l : p)

parseLs p = do 
  string "ls\n"
  items <- many $
    (Left <$> parseDir p) +++ (Right <$> parseFile p)
  let (dirs,files) = partitionEithers items
  pure $ Ls p dirs files 

cd _ (Cd p) = p 
cd p _ = p

parseCommands p = (eof $> []) +++ do 
  string "$ "
  cmd <- parseCd p +++ parseLs p 
  (cmd :) <$> parseCommands (cd p cmd)

parse = readp $ do 
  parseCommands []

fileSize (File n _) = n
dirSize (Dir n _ _) = n

makeDisk :: [Cmd] -> Map.Map Path Dir
makeDisk = foldr' (flip go) Map.empty 
  where
    reify disk ds = (disk !) <$> ds

    go disk (Cd _) = disk
    go disk (Ls p ds fs) = 
      Map.insert p (Dir size ds' fs) disk
      where 
        ds' = reify disk ds
        size = sum $ (dirSize <$> ds') ++ (fileSize <$> fs)

one = sum . filter (<= 100000) . fmap dirSize . Map.elems

total = 70000000

two disk = filter (>= need) . sort . fmap dirSize . Map.elems $ disk
  where 
    free = total - (dirSize $ disk ! [])
    need = 30000000 - free


main :: IO () 
main = do 
  d <- getContents 
  let inp = parse d
  let disk = makeDisk inp
  print $ inp
  print $ one disk
  print $ two disk