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
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), (<|), (><))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import System.Environment (getArgs)
import Text.Parsec (char, letter, parse, sepBy, spaces, string)
import Text.Parsec.Text (Parser)

data ModuleType = Conjunction | FlipFlop | BroadCaster deriving (Show)

type Node = T.Text

data Graph = Graph
  { outVertices :: Map Node (Seq Node),
    inVertices :: Map Node (Seq Node),
    vertexType :: Map Node ModuleType
  }
  deriving (Show)

transpose :: (Ord b, Foldable f, Applicative f, Monoid (f a)) => Map a (f b) -> Map b (f a)
transpose = M.foldrWithKey aux mempty
  where
    aux a bs m = foldl' (flip (M.alter (Just . maybe (pure a) (pure a <>)))) m bs

parser :: Parser Graph
parser =
  do
    defs <- some ((,,) <$> mtype <*> mname <* string " -> " <*> mdests <* spaces)
    let outVertices = Seq.fromList <$> M.fromList (map (\(a, b, c) -> (b, c)) defs)
    let inVertices = transpose outVertices
    let vertexType = M.fromList $ map (\(a, b, c) -> (b, a)) defs
    return $ Graph {outVertices, inVertices, vertexType}
  where
    mtype = asum [char '%' $> FlipFlop, char '&' $> Conjunction, pure BroadCaster]
    mname = T.pack <$> some letter
    mdests = mname `sepBy` string ", "

buttonPress graph states = propagate graph states [] (Seq.singleton (0, "broadcaster"))
  where
    propagate graph@Graph {outVertices, inVertices, vertexType} state trace seq =
      case Seq.viewl seq of
        EmptyL -> (trace, state)
        ((input, n) :< ns) ->
          let neighbours = M.findWithDefault mempty n outVertices
              trace' = (input, n) : trace
              pulse = case M.lookup n vertexType of
                (Just BroadCaster) -> Just input
                (Just FlipFlop) -> if input == 1 then Nothing else Just (1 - M.findWithDefault 0 n state)
                (Just Conjunction) ->
                  let memory = (\v -> M.findWithDefault 0 v state) <$> (inVertices ! n)
                   in if all (== 1) memory then Just 0 else Just 1
                _ -> Nothing
           in case pulse of
                Nothing -> propagate graph state trace' ns
                (Just p) -> propagate graph (M.insert n p state) trace' (ns >< ((p,) <$> neighbours))

part1 :: Graph -> IO ()
part1 graph = do
  let trace = fst $ iterate (\(t, s) -> let (t', s') = buttonPress graph s in (t ++ t', s')) ([], mempty) !! 1000
  print $ length (filter ((== 1) . fst) trace) * length (filter ((== 0) . fst) trace)

part2 :: Graph -> IO ()
part2 graph = do
  -- print $ map (Seq.length . snd) $ M.toList (outVertices graph)
  -- print $ map (Seq.length . snd) $ M.toList (inVertices graph)
  -- let traces = fst <$> iterate (\(_, s) -> buttonPress graph s) ([], mempty)
  -- print $ length $ takeWhile ((0, "rx") `notElem`) traces
  let states = snd <$> iterate (\(_, s) -> buttonPress graph s) ([], mempty)
  traverse_ print $ take 64 states

main = do
  graph <- fromRight (error "no parse") . parse parser "" <$> T.getContents
  part1 graph
  part2 graph
