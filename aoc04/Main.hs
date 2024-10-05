module Main where

import Control.Applicative
import Data.Either (fromRight)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (maybe)
import Data.Set qualified as S
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, parse, sepBy)
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.Text (Parser)

data Card = Card {winning :: [Int], numbers :: [Int]}
  deriving (Show, Eq, Ord)

parser :: Parser Card
parser = Card <$> (title *> numbers <* char '|' <* spaces) <*> numbers
  where
    title = string "Card" *> spaces *> some digit *> char ':' *> spaces
    numbers = many ((read <$> some digit) <* spaces)

wins :: Card -> Int
wins Card {winning, numbers} = let nums = S.fromList numbers in length $ filter (`S.member` nums) winning

part1 lines = print $ sum $ pow . wins . fromRight (error "no parse") . parse parser "" <$> lines
  where
    pow x = if x <= 0 then 0 else 2 ^ (x - 1)

part2 lines = do
  print $ sum $ M.elems $ foldl (scratch $ graph cards) counts indices
  where
    cards = fromRight (error "no parse") . parse parser "" <$> lines
    indices = [1 .. length cards]
    counts = M.fromList ((,1) <$> indices)
    graph cards = M.fromList [(idx, [idx + 1 .. idx + wins c]) | (idx, c) <- zip [1 ..] cards]
    scratch g counts idx =
      let count = counts ! idx
       in foldl (flip (M.alter (Just . maybe count (+ count)))) counts (g ! idx)

main = do
  lines <- many T.getLine
  part1 lines
  part2 lines
