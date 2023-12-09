module Main where

import Control.Applicative
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

parse :: T.Text -> [Int]
parse = map (read . T.unpack) . T.split (== ' ')

diff :: [Int] -> [Int]
diff ss = zipWith (-) (tail ss) ss

extrapolate :: [Int] -> Int
extrapolate ss
  | all (== 0) ss = 0
  | otherwise = let delta = extrapolate (diff ss) in last ss + delta

part1 :: IO ()
part1 = do
  lines <- many T.getLine
  print $ sum $ extrapolate . parse <$> lines

part2 :: IO ()
part2 = do
  lines <- many T.getLine
  print $ sum $ extrapolate . reverse . parse <$> lines

main = getArgs >>= run
  where
    run ["part1"] = part1
    run ["part2"] = part2
    run _ = error "Missing argument"
