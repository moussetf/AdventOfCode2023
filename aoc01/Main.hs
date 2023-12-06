{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

part1 :: IO ()
part1 = do
  lines <- many T.getLine
  print $ sum (getNumber <$> lines)
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

part2 :: IO ()
part2 = do
  lines <- many T.getLine
  print $ sum $ (\x -> read $ first x ++ last x) <$> lines
  where
    first = show . fromJust . firstDigit digitWords
    last = show . fromJust . firstDigit (T.reverse <$> digitWords) . T.reverse
    digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] :: [T.Text]

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
