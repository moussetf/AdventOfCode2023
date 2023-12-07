module Main where

import Control.Applicative
import Data.Either (fromRight)
import Data.Function
import Data.Functor
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Data.Ord qualified
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (Parsec, parse)
import Text.Parsec.Char (char, digit, spaces)
import Text.Parsec.Text (Parser)

newtype Hand = Hand {cards :: [Int]} deriving (Eq, Show)

instance Ord Hand where
  h1 <= h2
    | r1 == r2 = cards h1 <= cards h2
    | otherwise = r1 < r2
    where
      r1 = rank h1
      r2 = rank h2

rank :: Hand -> (Int, Int)
rank (Hand hand) = case lengths of
  [] -> (njokers, 0)
  [n] -> (n + njokers, 0)
  n : m : _ -> (n + njokers, m)
  where
    lengths = sortBy (comparing Data.Ord.Down) (map length $ group $ sort $ filter (/= -1) hand)
    njokers = length $ filter (== -1) hand

parser :: Bool -> Parser (Hand, Int)
parser withJoker = (,) <$> hand <* spaces <*> bid <* spaces
  where
    hand = Hand <$> some card
    card =
      asum
        [ char 'T' $> 10,
          char 'J' $> if withJoker then -1 else 11,
          char 'Q' $> 12,
          char 'K' $> 13,
          char 'A' $> 14,
          read . return <$> digit
        ]
    bid = read <$> some digit

part partNum = do
  lines <- many T.getLine
  let handsAndBids = fromRight (error "no parse") . parse (parser $ partNum == 2) "" <$> lines
  print $ sum $ zipWith (*) [1 ..] (map snd (sortBy (compare `on` fst) handsAndBids))

main = getArgs >>= run
  where
    run ["part1"] = part 1
    run ["part2"] = part 2
    run _ = error "Missing argument"
