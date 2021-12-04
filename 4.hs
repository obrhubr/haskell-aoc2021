import Data.List hiding (group)
import Data.Maybe

newtype Board = Board { 
    board :: [[(Int, Bool)]]
} deriving (Show)

trd :: (a, b, c) -> c
trd (a, b, c) = c

dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'
    
mapBoard :: ((Int, Bool) -> (Int, Bool)) -> Board -> Board
mapBoard func b = Board $ map (map func) (board b)

markNumber ::  Int -> Board -> Board
markNumber n = mapBoard (\ (x, y) -> if x == n then (x, True) else (x, y))

checkLine :: [(Int, Bool)] -> Bool
checkLine = all snd

checkBoard :: Board -> (Int, Int, Bool)
checkBoard b 
    | isJust hor = (fromJust hor, 0, True)
    | isJust vert = (fromJust vert, 1, True)
    | otherwise = (0, 0, False)
        where   hor = elemIndex True (map checkLine (board b))
                vert = elemIndex True (map checkLine (transpose $ board b))

applyNums :: [Int] -> Board -> (Int, Int, Int, Bool, Board)
applyNums [] b = (0, 0, 9999999999, False, b)
applyNums (n:ns) b = if z then (x, y, length ns, z, mb) else applyNums ns mb
    where   mb = markNumber n b
            (x, y, z) = checkBoard mb

getUnmarkedSum :: (Int, Int, Int, Bool, Board) -> Int
getUnmarkedSum (x, y, _, _, b) = sum $ map (sum . map fst) fb
    where   bs = if y == 0 then board b else transpose (board b)
            fb = map (filter (\(_, a) -> not a)) (board b)

part1 :: [Board] -> [Int] -> (Int, Int)
part1 boards nums = (getUnmarkedSum wb, nums !! (length nums - n - 1))
    where   mb = map (applyNums nums) boards
            wb@(_, _, n, _, _) = head . filter (\(_, _, _, a, _) -> a) $ sortBy (\(_, _, a, _, _) (_, _, b, _, _) -> compare b a) mb

part2 :: [Board] -> [Int] -> (Int, Int)
part2 boards nums = (getUnmarkedSum wb, nums !! (length nums - n - 1))
    where   mb = map (applyNums nums) boards
            wb@(_, _, n, _, _) = last . filter (\(_, _, _, a, _) -> a) $ sortBy (\(_, _, a, _, _) (_, _, b, _, _) -> compare b a) mb

main :: IO ()
main = do
    b <- map words . lines <$> readFile "./inputs/4_grids.txt"
    let boards = map (Board . map (map (\ x -> (read x :: Int, False)))) (group 5 $ dropEvery 6 b)
    nums <- map read . splitOn (==',') <$> readFile "./inputs/4_num.txt" :: IO [Int]
    print $ part2 boards nums
  