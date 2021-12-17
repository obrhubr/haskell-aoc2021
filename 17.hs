import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys, fromListWith, empty, insertWith )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set (toList, fromList)
import Data.Maybe ( fromJust, isJust )

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

getCoords :: [[String]] -> Map (Int, Int) Bool
getCoords coords = M.fromList [((a, b), True) | a <- [c1..c2], b <- [c3..c4]]
    where   c1 = read (head $ head coords) :: Int
            c2 = read (last $ head coords) :: Int
            c3 = read (head $ last coords) :: Int
            c4 = read (last $ last coords) :: Int

isTarget :: Map (Int, Int) Bool -> (Int, Int) -> Bool
isTarget m a = isJust (m M.!? a)

getY :: Map (Int, Int) Bool -> [Int] -> Int -> Int -> Int -> Int -> ([Int], Bool)
getY m traj vx vy y yv 
    | newpos < vy = (newpos : traj, isTarget m (vx, newpos))
    | otherwise = getY m (newpos : traj) vx vy newpos (yv - 1)
        where newpos = y + yv

getX :: Map (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
getX m (ex, ey) (px, py) (vx, vy)
    | nx > ex || ny < ey = False
    | isTarget m (nx, ny) = True
    | otherwise = getX m (ex, ey) (nx, ny) (nvx vx, nvy vy)
        where   (nx, ny) = (px + vx, py + vy)
                nvy yv = yv - 1
                nvx xv
                    | xv > 0 = xv - 1
                    | xv == 0 = 0
                    | xv < 0 = xv + 1

part1 :: Map (Int, Int) Bool -> (Int, Int)
part1 m = (maximum . fst $ vel !! idx, max - idx)
    where   validx = fst . fst . head $ M.toList m
            validy = snd . fst . last $ M.toList m
            vel = reverse $ map (getY m [] validx validy 0) [0..max] 
            max = 1000
            idx = fromJust (findIndex snd vel)

part2 :: Map (Int, Int) Bool  -> Int
part2 m = length $ filter (==True) l
    where   ex = fst . fst . last $ M.toList m
            ey = snd . fst . head $ M.toList m
            l = [getX m (ex, ey) (0, 0) (a, b) | a <- [- 302 .. 302], b <- [- 302 .. 302]]

main :: IO ()
main = do
    i <- lines <$> readFile "./inputs/17.txt"
    let coords = getCoords $ map (splitOn (==',')) i
    print $ part2 coords
