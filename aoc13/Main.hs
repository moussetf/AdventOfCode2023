{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bits (xor)
import Data.Containers.ListUtils (nubOrd)
import Data.List (find, inits, tails, transpose)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import System.Environment (getArgs)

-- Represent a pattern as two lists of ints (left-right and top-bottom)
data Pattern = Pattern [Int] [Int] deriving (Show)

parse :: T.Text -> Pattern
parse input = Pattern cols rows
  where
    cols = toInt <$> lines (T.unpack input)
    rows = toInt <$> transpose (lines $ T.unpack input)
    toInt = foldl (\n c -> 2 * n + if c == '#' then 1 else 0) 0

isMirrored as = not (null as) && even (length as) && as == reverse as

-- Get indices of horizontal lines of reflections
refls (Pattern rows cols) = (refl' rows, refl' cols)
  where
    refl' as = left as ++ right as
    left as = (length as -) . (`div` 2) . length <$> filter isMirrored (tails as)
    right as = (`div` 2) . length <$> filter isMirrored (inits as)

-- Get value of the set of reflections
values (a, b) = nubOrd $ ((100 *) <$> a) ++ b

-- Generate all expansions of the pattern obtained by flipping a single bit
expansions :: Pattern -> [Pattern]
expansions (Pattern rows cols) = [Pattern (flip' bit rows) (flip' (swap bit) cols) | bit <- bits]
  where
    bits = [(a, b) | a <- [0 .. length rows], b <- [0 .. length cols]]
    flip' (i, b) xs = (\(ix, x) -> if ix == i then x else (2 ^ b) `xor` x) <$> zip [0 ..] xs

main = do
  patterns <- map parse . T.splitOn "\n\n" <$> T.getContents
  print $ sum $ head . values . refls <$> patterns
  print $ sum $ altvalue <$> patterns
  where
    altvalue pattern =
      let [x] = values (refls pattern)
          modified = find (/= x) . values . refls <$> expansions pattern
       in head $ catMaybes modified
