{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

data Step = Insert T.Text Int | Remove T.Text

parseStep text = case T.split (`elem` ['-', '=']) text of
  [a, ""] -> Remove a
  [a, b] -> Insert a (read $ T.unpack b)

data Box = Box
  { lenses :: Map T.Text (Int, Int), -- rank and focal strength
    order :: Int
  }

process (Remove label) =
  M.adjust (\box -> box {lenses = M.delete label $ lenses box}) (hash label)
process (Insert label strength) = M.alter (Just . updateBox) (hash label)
  where
    updateBox Nothing = Box {lenses = M.singleton label (0, strength), order = 1}
    updateBox (Just (Box {lenses, order})) =
      if label `M.member` lenses
        then Box {lenses = M.adjust (\(r, _) -> (r, strength)) label lenses, order = order}
        else Box {lenses = M.insert label (order, strength) lenses, order = order + 1}

hash = T.foldl' (\val c -> ((val + ord c) * 17) `mod` 256) 0

focusingStrength = sum . map fs . M.toList
  where
    fs (boxnum, Box {lenses}) =
      let lenses' = snd <$> sort (M.elems lenses)
       in (1 + boxnum) * sum (zipWith (*) [1 ..] lenses')

main = do
  steps <- T.split (== ',') <$> T.getLine
  print $ sum $ hash <$> steps
  print $ focusingStrength $ foldl (flip process) mempty (parseStep <$> steps)
