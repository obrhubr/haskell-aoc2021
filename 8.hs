import Data.List
import Data.Map as M ( Map, fromList, (!) )

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

countSpecial :: String -> Bool
countSpecial s
    | l == 2 = True
    | l == 3 = True
    | l == 4 = True
    | l == 7 = True
    | otherwise = False
        where l = length s

wireMapping :: [String] -> Map String Int
wireMapping numbers =
  let withLength i = filter ((==) i . length) numbers
      containsAll s cs = all (`elem` s) cs
      with cs = filter (`containsAll` cs)
      without cs = filter (not . (`containsAll` cs))
      includedIn cs = filter (containsAll cs)
      [zeroChars] = without fiveChars $ withLength 6
      [oneChars] = withLength 2
      [twoChars] = without threeChars $ without fiveChars $ withLength 5
      [threeChars] = with sevenChars $ withLength 5
      [fourChars] = withLength 4
      [fiveChars] = includedIn sixChars $ withLength 5
      [sixChars] = without sevenChars $ withLength 6
      [sevenChars] = withLength 3
      [eightChars] = withLength 7
      [nineChars] = with threeChars $ withLength 6
   in M.fromList $ zip (map sort [zeroChars, oneChars, twoChars, threeChars, fourChars, fiveChars, sixChars, sevenChars, eightChars, nineChars]) [0 .. 9]

toNumber :: [Int] -> Int
toNumber = foldl (\i j -> 10 * i + j) 0

part1 :: [[[String]]] -> Int
part1 list = length $ filter countSpecial a
    where a = concat [a !! 1 | a <- list]

part2 :: [([String], [String])] -> Int
part2 list = sum $ map toNumber res
    where   res = map (uncurry getNumber) list
            nums = [fst a | a <- list]
            dispNums = [snd a | a <- list]
            getNumber a b = map ((wireMapping a !) . sort) b

main :: IO ()
main = do
  e <- lines <$> readFile "./inputs/8.txt"
  let es = map (splitOn (=='|')) e
  let entries = map (map words) es
  -- print $ part1 ess
  print $ part2 [(head a, a !! 1) | a <- entries]