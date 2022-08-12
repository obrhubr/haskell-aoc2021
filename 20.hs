import Data.List
import Data.Map as M ( fromList, Map, filterWithKey, (!?), (!), mapMaybe, toList, lookup, mapWithKey, member )
import Data.Char (digitToInt)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, insert, singleton, minView, member)
import Data.Maybe ( fromMaybe, isJust, fromJust, catMaybes, isNothing )
import qualified Control.Arrow as Data.Bifunctor

charToInt :: Char -> Int
charToInt s = if s == '#' then 1 else 0

makeGrid :: Int -> [String] -> M.Map (Int, Int) Int
makeGrid l rows = M.fromList
    [ ((x, y), charToInt ((rows !! y) !! x)) | x <- [0..l - 1], y <- [0..l - 1]]

makeString :: String -> M.Map Int Int
makeString s = M.fromList $ zip [0..] (map charToInt s)

neighbors :: Map (Int, Int) Int -> Int -> (Int, Int) -> [Int]
neighbors m deflt (x, y) = map (fromMaybe deflt) n
    where n =   [
                    m M.!? (x - 1, y - 1), m M.!? (x, y - 1), m M.!? (x + 1, y - 1),
                    m M.!? (x - 1, y), m M.!? (x, y), m M.!? (x + 1, y),
                    m M.!? (x - 1, y + 1), m M.!? (x, y + 1), m M.!? (x + 1, y + 1)
                ]

decimal :: [Int] -> Int
decimal xs = foldl' (\sum (pos,x) -> sum + x * 10 ^ (l - pos)) 0 $ zip [1..] xs where
            l = length xs

bintodec :: Int -> Int
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10

getBinary :: [Int] -> Int
getBinary i = bintodec $ decimal i

getCorresponding :: M.Map Int Int -> Int -> Int
getCorresponding m i = m M.! i

enhance :: Int -> Int -> Int -> M.Map Int Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
enhance offset l deflt s m = M.fromList $ [((x, y), getCorresponding s $ getBinary $ neighbors m deflt (x, y)) | x <- [- offset .. l + offset], y <- [-offset .. l + offset]]

loopEnhance _ 50 m _ = m
loopEnhance l n m s = loopEnhance l (n + 1) nm s
    where nm = enhance ((n + 1) * 2) l (n `mod` 2) s m

part1 :: Int -> Map (Int, Int) Int -> Map Int Int -> Int
part1 l m s = length $ filter ((==1) . snd) (M.toList $ enhance 4 l 1 s $ enhance 2 l 0 s m)

part2 :: Int -> Map (Int, Int) Int -> Map Int Int -> Int
part2 l m s = length $ filter ((==1) . snd) grid
    where grid = M.toList $ loopEnhance l 0 m s

showGrid :: Int -> M.Map (Int, Int) Int -> String
showGrid l points = unlines $
  [ [ if points M.! (x,y) == 1 then '#' else '.' | x <- [0..l - 1] ]
  | y <- [0..l - 1]
  ]

main :: IO ()
main = do
    g <- lines <$> readFile "./inputs/20.txt"
    s <- readFile "./inputs/20_string.txt"
    let l = length (head g)
    let grid = makeGrid l g
    let str = makeString s
    print $ part2 l grid str