import Data.Char (digitToInt)
import Data.Foldable
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (maybe)
import Data.Set qualified as S
import System.Environment (getArgs)

data Direction = N | S | W | E deriving (Eq, Show, Ord)

(m, n) ~> N = (m - 1, n)
(m, n) ~> S = (m + 1, n)
(m, n) ~> W = (m, n - 1)
(m, n) ~> E = (m, n + 1)

parse :: String -> Map (Int, Int) Int
parse = index . lines
  where
    index :: [String] -> Map (Int, Int) Int
    index xs = M.fromList $ concat $ zipWith (\line i -> map (\(j, c) -> ((i, j), digitToInt c)) line) (zip [0 ..] <$> xs) [0 ..]

dimensions board = (maximum $ fst <$> M.keys board, maximum $ snd <$> M.keys board)

type Node = ((Int, Int), Direction, Int)

dijkstra :: Map (Int, Int) Int -> [Node] -> Map Node [Node] -> Map Node Int
dijkstra board start nbs = dijkstra' mempty (S.fromList [(board ! p, s) | s@(p, _, _) <- start])
  where
    dijkstra' dist cand = case S.lookupMin cand of
      Nothing -> dist
      (Just (d, u)) ->
        let cand' = S.delete (d, u) cand
         in if u `M.member` dist
              then dijkstra' dist cand'
              else
                dijkstra'
                  (M.alter (Just . maybe d (min d)) u dist)
                  (foldl' (flip S.insert) cand' $ (\u@(p, _, _) -> (board ! p + d, u)) <$> nbs ! u)

toGraph board = M.fromList $ map (\u -> (u, neighbors u)) nodes
  where
    nodes = (,,) <$> M.keys board <*> [N, E, W, S] <*> [0 .. 2]
    neighbors (p, d, r) = aux p (straight d r ++ turns d r)
    turns d r
      | d `elem` [E, W] = [(N, 0), (S, 0)]
      | d `elem` [N, S] = [(E, 0), (W, 0)]
    straight d r = [(d, r + 1) | r < 2]
    aux p = filter (\(p, _, _) -> p `M.member` board) . map (\(d, r) -> (p ~> d, d, r))

toGraph' board = M.fromList $ map (\u -> (u, neighbors u)) nodes
  where
    nodes = (,,) <$> M.keys board <*> [N, E, W, S] <*> [0 .. 9]
    neighbors (p, d, r) = aux p (straight d r ++ turns d r)
    turns d r
      | d `elem` [E, W] = if r <= 2 then [] else [(N, 0), (S, 0)]
      | d `elem` [N, S] = if r <= 2 then [] else [(E, 0), (W, 0)]
    straight d r = [(d, r + 1) | r < 9]
    aux p = filter (\(p, _, _) -> p `M.member` board) . map (\(d, r) -> (p ~> d, d, r))

part1 :: IO ()
part1 = do
  board <- parse <$> getContents
  let (h, w) = dimensions board
  let dist = dijkstra board [((0, 1), E, 0), ((1, 0), S, 0)] (toGraph board)
  print $ minimum $ (dist !) <$> filter (\(p, _, _) -> p == (h, w)) (M.keys dist)

part2 :: IO ()
part2 = do
  board <- parse <$> getContents
  let (h, w) = dimensions board
  let dist = dijkstra board [((0, 1), E, 0), ((1, 0), S, 0)] (toGraph' board)
  print $ minimum $ (dist !) <$> filter (\(p, _, r) -> p == (h, w) && r >= 3) (M.keys dist)

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
