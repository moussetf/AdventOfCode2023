{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Foldable
import Data.Functor
import Data.List (intercalate, scanl')
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (maybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Internal.Fusion.Size (isEmpty)
import Debug.Trace
import System.Environment (getArgs)
import Text.Parsec (char, letter, parse, sepBy, spaces, string)
import Text.Parsec.Text (Parser)

data ModuleType = Conjunction | FlipFlop | BroadCaster deriving (Show)

type Node = T.Text

type Graph = Map Node ([Node], [Node], ModuleType)

transpose :: (Ord b, Foldable f, Applicative f, Monoid (f a)) => Map a (f b) -> Map b (f a)
transpose = M.foldrWithKey aux mempty
  where
    aux a bs m = foldl' (flip (M.alter (Just . maybe (pure a) (pure a <>)))) m bs

parser :: Parser Graph
parser =
  do
    defs <- some ((,,) <$> mtype <*> mname <* string " -> " <*> mdests <* spaces)
    let outVertices = M.fromList (map (\(a, b, c) -> (b, c)) defs)
    let inVertices = transpose outVertices
    let vertexType = M.fromList $ map (\(a, b, c) -> (b, a)) defs
    return $ M.fromList $ (\n -> (n, (M.findWithDefault [] n outVertices, M.findWithDefault [] n inVertices, vertexType ! n))) <$> M.keys vertexType
  where
    mtype = asum [char '%' $> FlipFlop, char '&' $> Conjunction, pure BroadCaster]
    mname = T.pack <$> some letter
    mdests = mname `sepBy` string ", "

data Fifo a = Fifo [a] [a]

view (Fifo [] []) = Nothing
view (Fifo [] back) = view $ Fifo (reverse back) []
view (Fifo (x : xs) back) = Just (x, Fifo xs back)

push as (Fifo front back) = Fifo front (as ++ back)

buttonPress graph state = propagate state [] (Fifo [] [(0, T.pack "broadcaster")])
  where
    propagate state trace seq = case view seq of
      Nothing -> (trace, state)
      Just (signal, seq') -> let (state', pulses) = process signal state in propagate state' (signal : trace) (push pulses seq')
    process (input, n) state
      | n == "rx" = (state, [])
      | n `elem` ["qt"] = traceShow (T.unpack n ++ " " ++ show input) (state, [])
      | otherwise =
          let (neighbours, inVertices, vertexType) = graph ! n
              pulse = case vertexType of
                BroadCaster -> Just input
                FlipFlop -> if input == 1 then Nothing else Just (1 - M.findWithDefault 0 n state)
                Conjunction ->
                  let memory = (\v -> M.findWithDefault 0 v state) <$> inVertices
                   in if all (== 1) memory then Just 0 else Just 1
              handle Nothing _ = (state, [])
              handle (Just p) neighbours
                | n == "rx" && traceShow p False = (M.insert n p state, (p,) <$> toList neighbours)
                | otherwise = (M.insert n p state, (p,) <$> toList neighbours)
           in handle pulse neighbours

part1 :: Graph -> IO ()
part1 graph = do
  let trace = fst $ iterate (\(t, s) -> let (t', s') = buttonPress graph s in (t ++ t', s')) ([], mempty) !! 1000
  print $ length (filter ((== 1) . fst) trace) * length (filter ((== 0) . fst) trace)

part2 :: Graph -> IO ()
part2 graph = do
  -- print $ map (Seq.length . snd) $ M.toList (outVertices graph)
  -- print $ map (Seq.length . snd) $ M.toList (inVertices graph)
  let trace = iterate (\(t, s) -> let (t', s') = buttonPress graph s in (t', s')) ([], mempty)
  traverse_ (\(t, s) -> if t == [] then print "asd" else return ()) trace

-- print $ length $ takeWhile ((0, "rx") `notElem`) (snd <$> results)

main = do
  graph <- fromRight (error "no parse") . parse parser "" <$> T.getContents
  -- part1 graph
  -- part2 graph
  print ""
