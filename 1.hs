import Data.List

isBigger :: Int -> Int -> Int
isBigger a b
  | b > a = 1
  | otherwise = 0

sumtriple :: (Int, Int, Int) -> Int
sumtriple (a, b, c) = a + b + c

part1 :: [Int] -> Int
part1 nums = sum [if b > a then 1 else 0 | (a, b) <- zip nums (tail nums)]

part2 :: [Int] -> Int
part2 nums = do
  let offset = map sumtriple $ zip3 nums (tail nums) (tail (tail nums))
  let offset1 = map sumtriple $ zip3 (tail nums) (tail (tail nums)) (tail (tail (tail nums)))
  sum [if b > a then 1 else 0 | (a, b) <- zip offset offset1]

main :: IO ()
main = do
  entries <- map read . lines <$> readFile "./inputs/1.txt" :: IO [Int]
  print $ part1 entries
  