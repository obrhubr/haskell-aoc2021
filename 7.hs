import Data.List
import Data.Function
import Control.Arrow

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

median :: [Int] -> Int
median [] = 0
median [l] = l
median l = sort l !! (length l `div` 2)

average :: [Int] -> Int
average l = ceiling $ fromIntegral (sum l) / fromIntegral (length l)

part1 :: [Int] -> Int
part1 list = sum $ map (\x -> abs (x - med)) list
    where med = median list

part2 :: [Int] -> Int
part2 list = minimum [sum $ map (\x -> sum [1..abs (x - a)]) list | a <- [avg - 1..avg + 1]]
    where avg = average list

main :: IO ()
main = do
  entries <- map read . splitOn (==',') <$> readFile "./inputs/7.txt" :: IO [Int]
  print $ part2 entries