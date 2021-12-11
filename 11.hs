import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList )

makeGrid :: [String] -> M.Map (Int, Int) Int
makeGrid rows = M.fromList
    [ ((r, c), digitToInt n)
        |(r, row) <- zip [0 ..] rows
        , (c, n  ) <- zip [0 ..] row
    ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1), (x + 1, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1)]

coordList ::  [(Int, Int)]
coordList = [ (r, c) | r <- [0..9], c <- [0..9]]

increaseEnergy :: Map (Int, Int) Int -> Map (Int, Int) Int
increaseEnergy = mapWithKey (\k n -> n + 1)
        
increaseEnergyFlashes :: Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
increaseEnergyFlashes grid [] = grid
increaseEnergyFlashes grid (c:cl)
    | null cl && ce = ngrid
    | null cl && not ce = resetted
    | ce = increaseEnergyFlashes ngrid cl
    | otherwise = increaseEnergyFlashes resetted cl
        where   ce = grid M.! c > 9
                ngrid = mapWithKey (\k n -> if (k `elem` nc) && (n /= -1) then n + 1 else n) resetted
                nc = neighbors c
                resetted = if ce then M.insert c (-1) grid else grid

countFlashes :: Map (Int, Int) Int -> Int
countFlashes grid = length $ M.toList $ filterWithKey (\k n -> n == -1 || n > 9) grid

resetFlashes :: Map (Int, Int) Int -> Map (Int, Int) Int
resetFlashes = mapWithKey (\k n -> if n > 9 || n == -1 then 0 else n)

simulate :: Map (Int, Int) Int -> Int -> Int -> (Int, Int)
simulate grid t 0 = (t, 0)
simulate grid t n
    | flashCount == 100 = (t, n)
    | otherwise = simulate nngrid (t + flashCount) (n - 1)
        where   inc = increaseEnergy grid
                ngrid = increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes (increaseEnergyFlashes inc coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList) coordList
                nngrid = resetFlashes ngrid
                flashCount = countFlashes ngrid

part1 :: [String] -> Int
part1 grid = fst $ simulate (makeGrid grid) 0 100

part2 :: [String] -> Int
part2 grid = 10001 - snd (simulate (makeGrid grid) 0 10000)

main :: IO ()
main = do
    l <- lines <$> readFile "./inputs/11.txt"
    print $ part2 l