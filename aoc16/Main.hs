import Data.Bifunctor (first)
import Data.Char (ord)
import Data.Foldable
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import System.Environment (getArgs)

data Direction = N | S | W | E deriving (Eq, Show, Ord)

type Node = ((Int, Int), Direction)

type Graph = Map Node [Node]

parse :: String -> Map (Int, Int) Char
parse = index . lines
  where
    index xs = M.fromList $ concat $ zipWith (\xs a -> map (first (a,)) xs) (zip [0 ..] <$> xs) [0 ..]

toGraph :: Map (Int, Int) Char -> Graph
toGraph board = M.foldrWithKey' (\pos c g -> foldl' (addNeighbors c) g ((pos,) <$> [N, E, S, W])) mempty board
  where
    addNeighbors c graph u@(pos, dir) =
      let nodes = (filter (inside . fst) $ map (\dir -> (pos ~> dir, dir)) $ dirs c dir)
       in foldl' (addEdge u) (M.insert u [] graph) nodes
    addEdge u g v = M.adjust (v :) u g
    h = maximum $ fst <$> M.keys board
    w = maximum $ snd <$> M.keys board
    inside (m, n) = 0 <= m && m <= h && 0 <= n && n <= w
    (m, n) ~> N = (m - 1, n)
    (m, n) ~> S = (m + 1, n)
    (m, n) ~> W = (m, n - 1)
    (m, n) ~> E = (m, n + 1)
    dirs '.' d = [d]
    dirs '\\' d = case d of N -> [W]; W -> [N]; S -> [E]; E -> [S]
    dirs '/' d = case d of N -> [E]; E -> [N]; S -> [W]; W -> [S]
    dirs '-' d = if d `elem` [E, W] then [d] else [E, W]
    dirs '|' d = if d `elem` [N, S] then [d] else [N, S]

component graph u = component' mempty u
  where
    component' seen u = let seen' = S.insert u seen in foldl' component' seen' $ filter (not . (`S.member` seen')) (graph ! u)

part1 :: IO ()
part1 = do
  board <- parse <$> getContents
  print $ S.size $ S.map fst $ component (toGraph board) ((0, 0), E)

part2 :: IO ()
part2 = do
  board <- parse <$> getContents
  let h = maximum $ fst <$> M.keys board
  let w = maximum $ snd <$> M.keys board
  let border = concat $ [[((0, n), S), ((h, n), N)] | n <- [0 .. w]] ++ [[((m, 0), E), ((m, w), W)] | m <- [0 .. h]]
  print $ maximum $ S.size . S.map fst . component (toGraph board) <$> border

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
