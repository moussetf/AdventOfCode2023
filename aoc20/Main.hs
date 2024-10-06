{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Foldable
import Data.Functor
import Data.List (elemIndex, intercalate, scanl')
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (catMaybes, maybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Internal.Fusion.Size (isEmpty)
import Debug.Trace
import System.Environment (getArgs)
import Text.Parsec (char, letter, parse, sepBy, spaces, string)
import Text.Parsec.Text (Parser)

data ModuleType = Nand | FlipFlop | BroadCaster deriving (Show)

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
    mtype = asum [char '%' $> FlipFlop, char '&' $> Nand, pure BroadCaster]
    mname = T.pack <$> some letter
    mdests = mname `sepBy` string ", "

data Fifo a = Fifo [a] [a]

pop (Fifo [] []) = Nothing
pop (Fifo [] back) = pop $ Fifo (reverse back) []
pop (Fifo (x : xs) back) = Just (x, Fifo xs back)

push as (Fifo front back) = Fifo front (as ++ back)

buttonPress graph state = propagate state [] (Fifo [] [(0, T.pack "broadcaster")])
  where
    propagate state trace fifo = case pop fifo of
      Nothing -> (trace, state)
      Just (signal, fifo') ->
        let (state', pulses) = process signal state
         in propagate state' (signal : trace) (push pulses fifo')
    process (input, n) state = case M.lookup n graph of
      Nothing -> (state, [])
      Just (outV, inV, t) ->
        let pulse = case t of
              BroadCaster -> Just input
              FlipFlop -> if input == 1 then Nothing else Just (1 - M.findWithDefault 0 n state)
              Nand ->
                let memory = (\v -> M.findWithDefault 0 v state) <$> inV
                 in if all (== 1) memory then Just 0 else Just 1
            handle Nothing _ = (state, [])
            handle (Just p) neighbours = (M.insert n p state, (p,) <$> toList neighbours)
         in handle pulse outV

part1 :: Graph -> IO ()
part1 graph = do
  let trace = fst <$> concat (take 1001 $ fst <$> iterate (\(_, s) -> buttonPress graph s) ([], mempty))
  print $ length (filter (== 1) trace) * length (filter (== 0) trace)

-- \$ length (filter (\(s, _) -> s == 1) trace) * length (filter (\(s, _) -> s == 0) trace)

part2 :: Graph -> IO ()
part2 graph = do
  {- We have rx = NOT qt = AND mr kk gl bb.
     Moreover:
       bb sends 1 every 3967 steps, first in 43637
       mr sends 1 every 3907 steps, first in 46884
       kk sends 1 every 3931 steps, first in 47172
       gl sends 1 every 3989 steps, first in 43879
     We don't even need to chinese remainder it.
  -}
  print $ foldl lcm 1 [3967, 3907, 3931, 3989]

main = do
  graph <- fromRight (error "no parse") . parse parser "" <$> T.getContents
  part1 graph
  part2 graph
