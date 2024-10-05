{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Either (fromRight)
import Data.Functor
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (between, char, digit, letter, parse, sepBy, spaces, try)
import Text.Parsec.Text (Parser)

type Part = [Int]

type Box = [(Int, Int)]

type Volume = [Box]

volume :: Int -> Volume -> Int
volume dims v = sum $ map boxvol v
  where
    boxvol box = product $ map (\(a, b) -> abs $ b - a) $ take dims box

inside :: Part -> Volume -> Bool
p `inside` v = any (p `inside'`) v
  where
    p `inside'` box = and $ zipWith (\x (a, b) -> a <= x && x < b) p box

split :: Int -> Int -> Volume -> (Volume, Volume)
split dim n v = (below, above)
  where
    below = map (update (\(a, b) -> (a, min b n)) dim) v
    above = map (update (\(a, b) -> (max a n, b)) dim) v
    update f 0 (b : box) = f b : box
    update f dim (b : box) = b : update f (dim - 1) box

data Rule = Less Int Int T.Text | Greater Int Int T.Text | Goto T.Text

parser :: Parser (Map T.Text [Rule], [Part])
parser = ((,) . M.fromList <$> some (workflow <* spaces)) <*> some (part <* spaces)
  where
    part = bracketed ((letter *> char '=' *> int) `sepBy` char ',')
    workflow = (,) <$> wfname <*> bracketed (rule `sepBy` char ',')
    wfname = T.pack <$> some letter
    rule = try cmpRule <|> Goto <$> wfname
    cmpRule = do
      dim <- asum $ zipWith (\c dim -> char c $> dim) "xmas" [0 ..]
      cmp <- asum [char '<' $> Less, char '>' $> Greater]
      cmp dim <$> int <* char ':' <*> wfname
    int = read <$> some digit
    bracketed = between (char '{') (char '}')

accepted :: T.Text -> Map T.Text [Rule] -> Volume
accepted "A" _ = [repeat (1, 4001)]
accepted "R" _ = []
accepted s rules = accepted' s (rules ! s)
  where
    accepted' s (r : rs) =
      case r of
        (Goto s') -> accepted s' rules
        (Greater dim n s') ->
          let (_, v) = split dim (n + 1) $ accepted s' rules
              (v', _) = split dim (n + 1) $ accepted' s rs
           in v ++ v'
        (Less dim n s') ->
          let (v, _) = split dim n $ accepted s' rules
              (_, v') = split dim n $ accepted' s rs
           in v ++ v'

main = do
  (rules, parts) <- fromRight (error "no parse") . parse parser "" <$> T.getContents
  print $ sum $ concat $ filter (`inside` accepted "in" rules) parts
  print $ volume 4 $ accepted "in" rules
