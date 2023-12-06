module Main where

import Control.Applicative
import Data.Either (fromRight)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, parse)
import Text.Parsec.Char (digit, spaces, string)
import Text.Parsec.Text (Parser)

parser :: Parser [(Int, Int)]
parser = zip <$> times <*> distances
  where
    times = string "Time:" *> spaces *> many number
    distances = string "Distance:" *> spaces *> many number
    number = read <$> some digit <* spaces

parser' :: Parser (Int, Int)
parser' = (,) <$> time <*> distance
  where
    time = string "Time:" *> spaces *> number
    distance = string "Distance:" *> spaces *> number
    number = read <$> some (digit <* spaces)

solve :: Int -> Int -> Int
solve t r
  | d < 0 = 0
  | otherwise = 1 + b - a
  where
    d = t ^ 2 - 4 * r
    a = floor $ (fromIntegral t - sqrt (fromIntegral d)) / 2 + 1
    b = ceiling $ (fromIntegral t + sqrt (fromIntegral d)) / 2 - 1

part1 :: IO ()
part1 = do
  input <- T.getContents
  let races = fromRight (error "no parse") $ parse parser "" input
  print $ product $ uncurry solve <$> races

part2 :: IO ()
part2 = do
  input <- T.getContents
  let race = fromRight (error "no parse") $ parse parser' "" input
  print $ uncurry solve race

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
