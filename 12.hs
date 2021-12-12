import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys, fromListWith, empty, insertWith )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, empty, Set, insert )

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

linesToMap :: [String] -> Map String [String]
linesToMap l = M.fromListWith (++) $ concatMap (\[a, b] -> [(a, [b]), (b, [a])]) pairs
    where pairs = map (splitOn (=='-')) l

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

dfs :: (String -> [String] -> Bool ) -> Map String [String] -> [String] -> String -> [[String]]
dfs func graph visited node
    | node == "end" = [visited]
    | otherwise = concatMap (\p -> dfs func graph (p : visited) p) next
        where   next = [p | p <- graph M.! node, p /= "start", not (all isLower p) || func p visited]

part1 :: [String] -> Int
part1 l = length $ dfs func (linesToMap l) [] "start"
    where func p v = p `notElem` v

part2 :: [String] -> Int
part2 l = length $ dfs func (linesToMap l) [] "start"
    where func p v = not (hasDuplicates (filter (all isLower) v)) || (p `notElem` v)

main :: IO ()
main = do
    l <- lines <$> readFile "./inputs/12.txt"
    print $ part2 l