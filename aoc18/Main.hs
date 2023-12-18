import Control.Applicative
import Data.Char (intToDigit)
import Data.Either (fromRight)
import Data.Functor
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Parsec (char, count, digit, hexDigit, parse, spaces, string)
import Text.Parsec.Text (Parser)

type Polygon = [(Int, Int)]

parser :: Bool -> Parser Polygon
parser part1 = scanl aux (0, 0) <$> some (if part1 then numdir <* numdir' else numdir *> numdir')
  where
    numdir = (,) <$> direction <* spaces <*> number <* spaces
    numdir' = string "(#" *> (flip (,) <$> number' <*> direction') <* char ')' <* spaces
    direction = asum (char <$> "RDLU")
    direction' = asum (zipWith (\i d -> char (intToDigit i) $> d) [0 ..] "RDLU")
    number = read <$> some digit
    number' = read . ("0x" ++) <$> count 5 hexDigit
    aux (a, b) ('R', n) = (a + n, b)
    aux (a, b) ('L', n) = (a - n, b)
    aux (a, b) ('U', n) = (a, b - n)
    aux (a, b) ('D', n) = (a, b + n)

area :: Polygon -> Int
area ps = area + border
  where
    area = sum (zipWith det ps (tail ps)) `div` 2
    det (a, b) (c, d) = a * d - b * c
    border = 1 + sum (zipWith dist ps (tail ps)) `div` 2
    dist (a, b) (c, d) = abs (a - c) + abs (b - d)

part :: Int -> IO ()
part partNum = do
  polygon <- fromRight (error "no parse") . parse (parser (partNum == 1)) "" <$> T.getContents
  print $ area polygon

main = getArgs >>= run
  where
    run ["part1"] = part 1
    run ["part2"] = part 2
    run _ = error "Missing argument"
