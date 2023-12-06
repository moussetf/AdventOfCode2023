module Main where

import Control.Applicative
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.Map (Map, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, maybe)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, getPosition, parse)
import Text.Parsec.Char (anyChar, char, digit, newline, satisfy)
import Text.Parsec.Pos (sourceColumn, sourceLine)
import Text.Parsec.Text (Parser)

data Number = Number {value :: Int, startPos :: (Int, Int), len :: Int}
  deriving (Show, Eq, Ord)

data Symbol = Gear {pos :: (Int, Int)} | Other {pos :: (Int, Int)}
  deriving (Show, Eq, Ord)

currentPos = (,) <$> sourceLine <*> sourceColumn <$> getPosition

symbolParser :: Parser [Symbol]
symbolParser = catMaybes <$> many maybeSymbol
  where
    maybeSymbol = garbage <|> gear <|> other
    garbage = Nothing <$ (newline <|> char '.' <|> digit)
    gear = Just . Gear <$> (currentPos <* char '*')
    other = Just . Other <$> (currentPos <* anyChar)

numberParser :: Parser [Number]
numberParser = garbage *> some (number <* garbage)
  where
    number = do
      pos <- currentPos
      n <- some digit
      return $ Number {value = read n, startPos = pos, len = length n}
    garbage = many (satisfy (not . isDigit))

adjacents :: [Symbol] -> [Number] -> [[Number]]
adjacents symbols numbers = group $ concatMap process numbers
  where
    smap = M.fromList [(pos s, s) | s <- symbols]
    process number = (,number) <$> filter (`M.member` smap) (neighbors number)
    group = M.elems . foldl (\m (pos, number) -> M.alter (Just . maybe [number] (number :)) (smap ! pos) m) mempty
    neighbors (Number {startPos = (i, j), len}) =
      concat
        [ [(i + di, j + s) | di <- [-1, 1], s <- [0 .. len - 1]],
          [(i + di, j - 1) | di <- [-1 .. 1]],
          [(i + di, j + len) | di <- [-1 .. 1]]
        ]

part1 :: IO ()
part1 = do
  input <- T.getContents
  let numbers = fromRight (error "no parse") $ parse numberParser "" input
  let symbols = fromRight (error "no parse") $ parse symbolParser "" input
  print $ sum $ value <$> nubOrd (concat $ adjacents symbols numbers)

part2 :: IO ()
part2 = do
  input <- T.getContents
  let numbers = fromRight (error "no parse") $ parse numberParser "" input
  let gears = filter isGear $ fromRight (error "no parse") $ parse symbolParser "" input
  print $ sum $ product . map value <$> filter ((== 2) . length) (adjacents gears numbers)
  where
    isGear (Gear _) = True
    isGear (Other _) = False

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
