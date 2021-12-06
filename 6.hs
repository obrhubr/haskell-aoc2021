import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

listToBucket :: [Int] -> [Int]
listToBucket l = [ count x l | x <- reverse [0..8]]

simulate :: Int -> [Int] -> [Int]
simulate 0 l = l
simulate n l = simulate (n - 1) ([ if i == 6 then x + last l else x | (x, i) <- zip (take 9 (last l : l)) (reverse [0..8])])

part1 :: [Int] -> Int
part1 list = sum $ simulate 80 (listToBucket list)

part2 :: [Int] -> Int
part2 list = sum $ simulate 256 (listToBucket list)

main :: IO ()
main = do
  entries <- map read . splitOn (==',') <$> readFile "./inputs/6.txt" :: IO [Int]
  print $ part2 entries