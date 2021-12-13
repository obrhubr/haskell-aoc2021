import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys, fromListWith, empty, insertWith )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, empty, Set, insert, map, member, toList, size )

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

showGrid :: Set.Set (Int, Int) -> String
showGrid points = unlines $
  [ [ if (x, y) `Set.member` points then 'x' else ' ' | x <- [0 .. w] ]
  | y <- [0 .. h]
  ]
  where (h, w) = (maximum $ Set.map snd points, maximum $ Set.map fst points)

foldGrid :: (Char, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
foldGrid fd = Set.map (foldpoint fd)
    where   foldpoint ('x', l) (x, y) = if l >= x then (x, y) else (l - (x - l), y)
            foldpoint ('y', l) (x, y) = if l >= y then (x, y) else (x, (l - (y - l)))

execFolds :: [(Char, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
execFolds [] grid = grid
execFolds (f:fs) grid = execFolds fs ngrid
    where ngrid = foldGrid f grid

part1 :: (Char, Int) -> Set.Set (Int, Int) -> Int
part1 f g = Set.size $ foldGrid f g

part2 :: [(Char, Int)] -> Set.Set (Int, Int) -> String
part2 f g = showGrid $ execFolds f g

main :: IO ()
main = do
    c <- lines <$> readFile "./inputs/13.txt"
    let cs = Prelude.map (splitOn (==',')) c
    let coords = [(read (head a) :: Int, read (a !! 1) :: Int) | a <- cs]
    let grid = Set.fromList coords

    f <- lines <$> readFile "./inputs/13_folds.txt"
    let folds = [(head $ head a, read (a !! 1) :: Int) | a <- Prelude.map (splitOn (=='=')) f]

    print $ part1 (head folds) grid
    putStr $ part2 folds grid