import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

mostCommonBit :: [String] -> String
mostCommonBit a = if count "1" a >= count "0" a then "1" else "0"

leastCommonBit :: [String] -> String
leastCommonBit a = if count "1" a >= count "0" a then "0" else "1"

getColumn :: [String] -> Int -> [String]
getColumn a n = [ [bit] | bit <- map (!! n) a ]

getCommonBits :: [String] -> [String]
getCommonBits a = [ mostCommonBit $ getColumn x y | (x, y) <- zip (repeat a) [0..length (head a) - 1]]

invertBin :: [String] -> [String]
invertBin = map (\x -> if "1" == x then "0" else "1")

part1 :: [String] -> ([String], [String])
part1 nums = (cb, invertBin cb)
    where cb = getCommonBits nums

filterBits :: ([String] -> String) -> [String] -> Int -> [String]
filterBits _ [a] _ = [a]
filterBits func a 0 = filter (\x -> func (getColumn a (length a)) == [last x]) a
filterBits func a n = filterBits func (filter (\x -> cb == [x !! ((length x - 1) - n)]) a) (n-1)
    where cb = func $ getColumn a ((length (head a) - 1) - n)

part2 :: [String] -> ([String], [String])
part2 nums = (filterBits mostCommonBit nums (length (head nums) - 1), filterBits leastCommonBit nums (length (head nums) - 1))

main :: IO ()
main = do
  entries <- lines <$> readFile "./inputs/3.txt"
  print $ part2 entries
  