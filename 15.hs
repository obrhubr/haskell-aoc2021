import Data.List
import Data.Map as M ( fromList, Map, filterWithKey, (!?), (!), mapMaybe, toList, lookup )
import Data.Char (digitToInt)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, insert, singleton, minView, member)
import Data.Maybe ( fromMaybe, isJust, fromJust )
import qualified Control.Arrow as Data.Bifunctor

data Path a = Path {cost :: Int , trajectory :: [(a, a)]}
    deriving (Show)

instance Eq (Path a) where
    a == b = cost a == cost b

instance Ord (Path a) where
    compare a b = compare (cost a) (cost b)

makeGrid :: [String] -> M.Map (Int, Int) Int
makeGrid rows = M.fromList
    [ ((r, c), digitToInt n)
        |(r, row) <- zip [0 ..] rows
        , (c, n  ) <- zip [0 ..] row
    ]

increaseGridByN :: Int -> [String] -> [String]
increaseGridByN n = map (map (\x -> head $ show ((((digitToInt x + n) - 1) `mod` 9) + 1)))

repeatGrid :: [String] -> [String]
repeatGrid grid = concat [increaseGridByN x line | x <- [0..4]]
    where line = map concat $ transpose [increaseGridByN x grid | x <- [0..4]]

dijkstra
    :: (Ord cost , Ord node)
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> node                               -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost , vertex) , withoutVertex)
                | vertex == target            -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)

neighbors :: Map (Int, Int) Int -> (Int, Int) -> [(Int, (Int, Int))]
neighbors m (x, y) = map (Data.Bifunctor.first fromJust) (filter (\(x, y) -> isJust x) [(m M.!? (x - 1, y), (x - 1, y)), (m M.!? (x, y - 1), (x, y - 1)), (m M.!? (x + 1, y), (x + 1, y)), (m M.!? (x, y + 1), (x, y + 1))])

getShortest :: Map (Int, Int) Int -> (Int, Int) -> Maybe (Path Int, (Int, Int))
getShortest m end = dijkstra step end (Path 0 [], (0,0))
    where   step :: (Path Int, (Int, Int)) -> [(Path Int, (Int, Int))]
            step (Path cost traj, node) =
                [ (Path (cost + edgeCost) (child : traj), child)
                | (edgeCost , child) <- neighbors m node
                ]

part1 :: [String] -> Maybe (Path Int, (Int, Int))
part1 l = getShortest (makeGrid l) (99, 99)

part2 :: [String] -> Maybe (Path Int, (Int, Int))
part2 l = getShortest (makeGrid $ repeatGrid l) (499, 499)

main :: IO ()
main = do
    l <- lines <$> readFile "./inputs/15.txt"
    print $ part2 l