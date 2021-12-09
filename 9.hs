import Data.List
import Data.Map as M ( fromList, Map, filterWithKey, (!?), (!), mapMaybe, toList)
import Data.Char (digitToInt)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList )

makeGrid :: [String] -> M.Map (Int, Int) Int
makeGrid rows = M.fromList
    [ ((r, c), digitToInt n)
        |(r, row) <- zip [0 ..] rows
        , (c, n  ) <- zip [0 ..] row
    ]

neighbors :: (Int, Int) -> Map Int (Int, Int)
neighbors (x, y) = M.fromList $ zip [0..4] [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

lowPoints :: Map (Int, Int) Int -> [((Int, Int), Int)]
lowPoints grid = M.toList $ M.filterWithKey (\p n -> all (> n) (M.mapMaybe (grid M.!?) (neighbors p))) grid

step :: Map (Int, Int) Int -> (Int, Int) -> Seq.Seq (Int, Int)
step grid p =
    Seq.fromList
        $ filter
            (\p' -> case grid M.!? p' of
                Nothing -> False
                Just n  -> n /= 9 && n > (grid M.! p)
            )
        $ map snd . M.toList $ neighbors p

bfs :: Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
bfs grid ps
    | null ps = []
    | otherwise = ps : concatMap (bfs grid) np
        where   np = F.toList $ step grid ps

part1 :: Map (Int, Int) Int -> Int
part1 grid = sum $ map ((+1) . snd) (lowPoints grid)

part2 :: Map (Int, Int) Int -> Int
part2 grid = product highest3
    where   basins = map (length . dedupe . bfs grid . fst) (lowPoints grid)
            dedupe = F.toList . Set.fromList
            highest3 = take 3 (sortBy (flip compare) basins)

main :: IO ()
main = do
    l <- lines <$> readFile "./inputs/9.txt"
    print $ part2 $ makeGrid l