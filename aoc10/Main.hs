module Main where

import Control.Applicative
import Data.Foldable
import Data.List (unfoldr)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

type Node = (Int, Int)

parse :: T.Text -> Map Node Char
parse text = M.fromList $ concat pairs
  where
    ls = T.unpack <$> T.lines text
    pairs = zipWith (\a -> map (\(b, t) -> ((a, b), t))) [0 ..] (zip [0 ..] <$> ls)

loop :: Map Node Char -> [Node]
loop m = start : unfoldr generator (start, mempty)
  where
    start = maybe (error "no animal") fst $ find ((== 'S') . snd) $ M.toList m
    generator = fmap (\(n, v) -> (n, (n, v))) . uncurry (next m)

-- Get next neighbor on the loop, given the set of visited vertices.
next :: Map Node Char -> Node -> Set Node -> Maybe (Node, Set Node)
next m node@(a, b) visited =
  case m ! node of
    '-' -> left <|> right
    '|' -> up <|> down
    'J' -> left <|> up
    'F' -> right <|> down
    'L' -> up <|> right
    '7' -> left <|> down
    'S' -> up <|> down <|> right <|> left
  where
    try syms n
      | n `M.member` m && not (n `S.member` visited) && (m ! n) `elem` syms = Just (n, S.insert node visited)
      | otherwise = Nothing
    right = try ['-', '7', 'J', 'S'] (a, b + 1)
    left = try ['-', 'F', 'L', 'S'] (a, b - 1)
    up = try ['|', 'F', '7', 'S'] (a - 1, b)
    down = try ['|', 'J', 'L', 'S'] (a + 1, b)

-- Partition map keys into inside and outside the loop
partition :: Map Node Char -> (Set Node, Set Node)
partition m = foldl aux (mempty, mempty) $ M.keys m
  where
    lp = loop m
    lpset = S.fromList lp
    aux (outside, inside) node
      | node `S.member` lpset = (outside, inside)
      | any (`S.member` outside) $ neighbors node = (S.insert node outside, inside)
      | any (`S.member` inside) $ neighbors node = (outside, S.insert node inside)
      | node `isInside` lp = (outside, S.insert node inside)
      | otherwise = (S.insert node outside, inside)
    neighbors (a, b) = [(a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1)]

isInside :: Node -> [Node] -> Bool
(a, b) `isInside` lp = abs (sum angles) > 3
  where
    offsets = [(x - a, y - b) | (x, y) <- lp]
    angles = zipWith angle offsets (tail offsets ++ offsets)
    angle u@(a, b) v@(c, d) = signum (det u v) * acos (u .* v / (norm u * norm v))
    det (a, b) (c, d) = fromIntegral $ a * d - b * c
    (a, b) .* (c, d) = fromIntegral $ a * c + b * d
    norm (a, b) = sqrt $ fromIntegral $ a * a + b * b

part1 :: IO ()
part1 = do
  input <- T.getContents
  print $ (length (loop $ parse input) + 1) `div` 2

part2 :: IO ()
part2 = do
  input <- T.getContents
  let (outside, inside) = partition (parse input)
  print $ S.size inside

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
