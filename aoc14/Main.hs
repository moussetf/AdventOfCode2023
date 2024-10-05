import Data.Bifunctor (first)
import Data.List (find, groupBy, mapAccumL)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import System.Environment (getArgs)

-- Boulders are represented as Gaussian integers
data Gaussian = Int :+: Int deriving (Eq, Ord)

instance Num Gaussian where
  (a :+: b) + (c :+: d) = (a + c) :+: (b + d)
  (a :+: b) - (c :+: d) = (a - c) :+: (b - d)
  (a :+: b) * (c :+: d) = (a * c - b * d) :+: (a * d + b * c)
  signum (a :+: b) = error "no sign"
  abs (a :+: b) = error "no abs"
  fromInteger x = fromInteger x :+: 0

type Matrix a = Map Gaussian a

-- A direction has an offset for every location (by how much the boulder there
-- would move if there were no other boulders) and a displacement value (by how
-- much the boulder is displaced if another boulder is already at its target
-- destination).
data Direction = Direction (Matrix Gaussian) Gaussian

directions chars = (north, west, south, east)
  where
    north = Direction (transform (-1) $ fmap (* (-1)) $ south' $ transform (-1) chars) (-i1)
    west = Direction (transform (-i1) $ fmap (* (-i1)) $ south' $ transform i1 chars) 1
    south = Direction (south' chars) i1
    east = Direction (transform i1 $ fmap (* i1) $ south' $ transform (-i1) chars) (-1)
    south' chars = M.fromList $ concat $ map processColumn <$> groupBy sameColumn $ M.toList chars
      where
        sameColumn (a :+: _, _) (c :+: _, _) = a == c
        processColumn = snd . mapAccumL (\off (z, c) -> if c == '#' then (0, (z, 0)) else (off - i1, (z, off))) 0
    transform z = M.mapKeys (* z)
    i1 = 0 :+: 1

-- Parse the input into a matrix of chars
parse = index . reverse . lines
  where
    index xs = M.fromList $ concat $ zipWith (\xs a -> map (first (:+: a)) xs) (zip [0 ..] <$> xs) [0 ..]

-- Find the period of a periodic sequence (assuming elements are otherwise distinct)
period xs = (\(a, b, _) -> (a, b)) <$> find (\(a, _, _) -> a >= 0) (scanl aux (-1, -1, mempty) (zip [0 ..] xs))
  where
    aux (_, _, seen) (i, x)
      | x `M.member` seen = (seen ! x, i - (seen ! x), seen)
      | otherwise = (-1, -1, M.insert x i seen)

-- Extract the set of boulder locations from a matrix of chars
boulders = S.fromList . map fst . filter ((== 'O') . snd) . M.toList

-- Weigh the north (upper half-plane) side of the platform
weight = sum . map (\(a :+: b) -> 1 + b) . S.toList

-- Roll all boulders into the given direction
roll (Direction offsets disp) bldrs = foldl (\bldrs bld -> place bldrs (bld + (offsets ! bld))) mempty $ S.toList bldrs
  where
    place bldrs bld
      | bld `S.member` bldrs = place bldrs (bld + disp)
      | otherwise = S.insert bld bldrs

part1 chars =
  let (n, w, s, e) = directions chars
   in print $ weight $ roll n $ boulders chars

part2 chars =
  print $ weight $ sequence !! (a + ((1000000000 - a) `mod` b))
  where
    (n, w, s, e) = directions chars
    sequence = iterate (roll e . roll s . roll w . roll n) $ boulders chars
    Just (a, b) = period sequence

main = do
  chars <- parse <$> getContents
  part1 chars
  part2 chars
