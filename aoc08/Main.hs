{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Either (fromRight)
import Data.Functor
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, alphaNum, parse)
import Text.Parsec.Char (char, spaces, string)
import Text.Parsec.Text (Parser)

data Dir = L | R deriving (Show)

parser :: Parser ([Dir], Map T.Text (T.Text, T.Text))
parser = (,) <$> some dir <* spaces <*> graph
  where
    dir = (char 'L' $> L) <|> (char 'R' $> R)
    graph = M.fromList <$> many conn
    conn = (,) <$> node <* string " = " <*> nodepair <* spaces
    nodepair = (,) <$> (char '(' *> node <* string ", ") <*> node <* char ')'
    node = T.pack <$> some alphaNum

period graph dirs start = length $ takeWhile ((/= 'Z') . T.last) $ scanl (move graph) start (cycle dirs)
  where
    move graph node L = fst (graph ! node)
    move graph node R = snd (graph ! node)

part1 :: IO ()
part1 = do
  input <- T.getContents
  let (dirs, graph) = fromRight (error "no parse") $ parse parser "" input
  print $ period graph dirs "AAA"

part2 :: IO ()
part2 = do
  input <- T.getContents
  let (dirs, graph) = fromRight (error "no parse") $ parse parser "" input
  print $ foldl lcm 1 $ period graph dirs <$> startNodes graph
  where
    startNodes = filter ((== 'A') . T.last) . M.keys

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
