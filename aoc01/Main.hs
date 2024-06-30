{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T

part1 lines = print $ sum $ getNumber <$> lines
  where
    getNumber = (\x -> read [T.head x, T.last x]) . T.filter isDigit

firstDigit :: [T.Text] -> T.Text -> Maybe Int
firstDigit digitWords text =
  case T.uncons text of
    Nothing -> Nothing
    Just (x, xs) ->
      ( let humanDigit = if isDigit x then Just $ read [x] else Nothing
            elfDigit = asum (zipWith (\w n -> if w `T.isPrefixOf` text then Just n else Nothing) digitWords [1 ..])
         in humanDigit <|> elfDigit <|> firstDigit digitWords xs
      )

part2 lines = print $ sum $ (\x -> read $ first x ++ last x) <$> lines
  where
    first = show . fromJust . firstDigit digitWords
    last = show . fromJust . firstDigit (T.reverse <$> digitWords) . T.reverse
    digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] :: [T.Text]

main = do
  lines <- many T.getLine
  part1 lines
  part2 lines
