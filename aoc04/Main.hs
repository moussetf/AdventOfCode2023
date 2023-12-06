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

part1 :: IO ()
part1 = do
  lines <- many T.getLine
  print $ sum $ pow . wins . fromRight (error "no parse") . parse parser "" <$> lines
  where
    pow x = if x <= 0 then 0 else 2 ^ (x - 1)

part2 :: IO ()
part2 = do
  lines <- many T.getLine
  let cards = fromRight (error "no parse") . parse parser "" <$> lines
  let indices = [1 .. length cards]
  let counts = M.fromList ((,1) <$> indices)
  print $ sum $ M.elems $ foldl (scratch $ graph cards) counts indices
  where
    graph cards = M.fromList [(idx, [idx + 1 .. idx + wins c]) | (idx, c) <- zip [1 ..] cards]
    scratch g counts idx = foldl (flip (M.alter (Just . maybe count (+ count)))) counts (g ! idx)
      where
        count = counts ! idx

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
