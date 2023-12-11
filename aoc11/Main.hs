module Main where

import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

type Pos = (Int, Int)

type Matrix a = Map Pos a

parse :: T.Text -> Matrix Char
parse text = M.fromList $ index2d (lines $ T.unpack text)
  where
    index2d xs = concat (zipWith (\a -> map (\(b, t) -> ((a, b), t))) [0 ..] (zip [0 ..] <$> xs))

emptyRows :: Matrix Char -> Set Int
emptyRows m = S.fromList $ filter isEmpty [h0 .. h1]
  where
    isEmpty h = all ((== '.') . (m !) . (h,)) [w0 .. w1]
    h0 = minimum $ fst <$> M.keys m
    h1 = maximum $ fst <$> M.keys m
    w0 = minimum $ snd <$> M.keys m
    w1 = maximum $ snd <$> M.keys m

galaxies :: Matrix Char -> [Pos]
galaxies m = map fst $ filter ((== '#') . snd) $ M.toList m

dist :: Int -> Set Int -> Set Int -> Pos -> Pos -> Int
dist scale rows cols (a, b) (c, d) = (dr + abs (a - c)) + (dc + abs (b - d))
  where
    nbetween s a b = length (filter (`S.member` s) [min a b + 1 .. max a b - 1])
    dr = (scale - 1) * nbetween rows a c
    dc = (scale - 1) * nbetween cols b d

transpose :: Matrix a -> Matrix a
transpose = M.mapKeys (\(a, b) -> (b, a))

part1 :: IO ()
part1 = do
  m <- parse <$> T.getContents
  let (gals, rows, cols) = (galaxies m, emptyRows m, emptyRows $ transpose m)
  print $ sum (dist 2 rows cols <$> gals <*> gals) `div` 2

part2 :: IO ()
part2 = do
  m <- parse <$> T.getContents
  let (gals, rows, cols) = (galaxies m, emptyRows m, emptyRows $ transpose m)
  print $ sum (dist 1000000 rows cols <$> gals <*> gals) `div` 2

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
