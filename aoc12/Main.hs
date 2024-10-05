import Control.Applicative
import Data.List (intercalate)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

arra ss ns = fst $ arraMemo mempty (ss ++ ".") ns
  where
    arraMemo m ss ns =
      let key = (length ss, length ns)
       in if key `M.member` m
            then (m ! key, m)
            else (let (v, m') = arra' m ss ns in (v, M.insert key v m'))
    arra' m ss [] = (if '#' `notElem` ss then 1 else 0, m)
    arra' m [] ns = (0, m)
    arra' m (s : ss) (n : ns)
      | s == '#' && run (n - 1) `prefixOf` ss = arraMemo m (drop n ss) ns
      | s == '.' = arraMemo m ss (n : ns)
      | s == '?' =
          let (a, m') = arraMemo m ss (n : ns)
              (b, m'') = if run (n - 1) `prefixOf` ss then arraMemo m' (drop n ss) ns else (0, m')
           in (a + b, m'')
      | otherwise = (0, m)
    run n = replicate n '#' ++ "."

[] `prefixOf` _ = True
(x : xs) `prefixOf` [] = False
(x : xs) `prefixOf` (s : ss)
  | s == x || s == '?' = xs `prefixOf` ss
  | otherwise = False

parse :: T.Text -> (String, [Int])
parse line = let [a, b] = T.split (== ' ') line in (T.unpack a, read . T.unpack <$> T.split (== ',') b)

main = do
  lines <- fmap parse <$> many T.getLine
  print $ sum $ uncurry arra <$> lines
  print $ sum $ uncurry arra . expand <$> lines
  where
    expand (a, b) = (intercalate "?" $ replicate 5 a, concat $ replicate 5 b)
