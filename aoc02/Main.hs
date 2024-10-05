module Main where

import Control.Applicative
import Data.Either (fromRight)
import Data.Functor
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, parse, sepBy1, spaces)
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.Text (Parser)

data Game = Game {index :: Int, rounds :: [[Count]]}
  deriving (Show)

data Count = Red Int | Blue Int | Green Int
  deriving (Show)

parser :: Parser Game
parser = Game <$> index <*> rounds
  where
    index = string "Game" *> spaces *> (read <$> some digit) <* char ':' <* spaces
    rounds = round `sepBy1` (char ';' *> spaces)
    round = count `sepBy1` (char ',' *> spaces)
    count = do
      num <- read <$> some digit <* spaces
      result <- (string "green" $> Green num) <|> (string "red" $> Red num) <|> (string "blue" $> Blue num)
      spaces
      return result

part1 lines = print $ sum $ value <$> games
  where
    games = fromRight (error "no parse") . parse parser "" <$> lines
    value (Game index rounds)
      | all (all isPossible) rounds = index
      | otherwise = 0
    isPossible (Red n) = n <= 12
    isPossible (Green n) = n <= 13
    isPossible (Blue n) = n <= 14

part2 lines = print $ sum $ power <$> games
  where
    games = fromRight (error "no parse") . parse parser "" <$> lines
    power (Game _ rounds) = let (r, g, b) = smallestCounts (concat rounds) in r * g * b
    smallestCounts = foldl aux (0, 0, 0)
    aux (r, g, b) (Red n) = (max r n, g, b)
    aux (r, g, b) (Green n) = (r, max g n, b)
    aux (r, g, b) (Blue n) = (r, g, max b n)

main = do
  lines <- many T.getLine
  part1 lines
  part2 lines
