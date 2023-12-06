module Main where

import Control.Applicative
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, parse)
import Text.Parsec.Char (digit, satisfy, spaces, string)
import Text.Parsec.Text (Parser)

data Range = Range {from :: Int, to :: Int}

parser :: Int -> Parser ([Range], [[(Int, Range)]])
parser partNum = (,) <$> (string "seeds: " *> many seed) <*> many transformation
  where
    seed = if partNum == 1 then (\n -> Range n (n + 1)) <$> number else range
    transformation = some (satisfy (not . isDigit)) *> some ((,) <$> number <*> range)
    number = read <$> some digit <* spaces
    range = (\n l -> Range n (n + l)) <$> number <*> number

move :: Range -> Range -> Int -> ([Range], [Range])
move (Range a b) (Range c d) dst = (moved, pending)
  where
    moved = cleanse [Range a c, Range (max a c + dst - c) (min b d + dst - c)]
    pending = cleanse [Range d b]
    cleanse = filter (\(Range a b) -> a < b)

transform :: [Range] -> [(Int, Range)] -> [Range]
transform seeds maps = transform' sortedSeeds sortedMaps []
  where
    sortedSeeds = sortBy (compare `on` from) seeds
    sortedMaps = sortBy (compare `on` (from . snd)) maps
    transform' seeds [] done = seeds ++ done
    transform' [] _ done = done
    transform' seeds@(s : ss) maps@((dst, m) : ms) done
      | to s <= from m = transform' ss maps (s : done)
      | to m <= from s = transform' seeds ms done
      | otherwise = let (moved, pending) = move s m dst in transform' (pending ++ ss) maps (moved ++ done)

part partNum = do
  input <- T.getContents
  let (seeds, transformations) = fromRight (error "no parse") $ parse (parser partNum) "" input
  print $ minimum $ from <$> foldl transform seeds transformations

main = getArgs >>= run
  where
    run ["part1"] = part 1
    run ["part2"] = part 2
    run _ = error "Missing argument"
