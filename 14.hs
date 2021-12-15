import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys, fromListWith, empty, insertWith )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set (toList, fromList)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

slideOverString :: M.Map String Char -> String -> String -> String
slideOverString m (x:xs:xss) rest
    | null xss = rest ++ (x : inserted : xs : [])
    | otherwise = slideOverString m (xs : xss) (rest ++ (x : inserted : []))
        where inserted = m M.! (x : xs : [])

applyRules :: M.Map String Char -> M.Map String Int -> M.Map String Int
applyRules m s = s

applyN :: M.Map String Char -> String -> Int -> String
applyN rules st n
    | n == 0 = st
    | otherwise = applyN rules rest (n - 1)
        where rest = slideOverString rules st ""

part1 :: M.Map String Char -> Int
part1 rules = highest - lowest
    where   endst = applyN rules "BVBNBVPOKVFHBVCSHCFO" 40
            elems = sort $ map (`count` endst) (Set.toList $ Set.fromList endst)
            lowest = head elems
            highest = last elems

main :: IO ()
main = do
    i <- lines <$> readFile "./inputs/14.txt"
    let ins = map (splitOn (==',')) i
    let rules = M.fromList $ [(head c, head (c !! 1)) | c <- ins]
    print $ part1 rules