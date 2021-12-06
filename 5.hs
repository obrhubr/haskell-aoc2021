import Data.List
import Data.Foldable (toList)
import qualified Data.Map.Strict as M

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

data Line = Line { _from, _to :: Coord } deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Grid a = M.Map Coord a
data Orientation = Vertical | Horizontal | Diagonal deriving (Show, Enum, Eq, Ord)

orientation :: Line -> Orientation
orientation (Line (x1, y1) (x2, y2)) 
    | x1 == x2 = Vertical
    | y1 == y2 = Horizontal
    | otherwise = Diagonal

pointsOnDiagonal :: Line -> [Coord]
pointsOnDiagonal (Line start@(x1, y1) (x2, y2)) =
  let len = abs (x1 - x2)
      (dx, dy) = (signum (x2 - x1), signum (y2 - y1))
      move (x, y) = (x + dx, y + dy)
  in take (len + 1) (iterate move start)

linesToGrid :: (Line -> [Coord]) -> [Line] -> Grid [Line]
linesToGrid diagHandler = foldl' addLines M.empty
    where addLines m l = case orientation l of 
            Vertical -> let Line (x, y1) (_, y2) = l
                            from = min y1 y2
                            to = max y1 y2
                            addPoint m y = M.insertWith (<>) (x, y) [l] m
                        in foldl' addPoint m [from..to]
            Horizontal ->   let Line (x1, y) (x2, _) = l
                                from = min x1 x2
                                to = max x1 x2
                                addPoint m x = M.insertWith (<>) (x, y) [l] m
                            in foldl' addPoint m [from..to]
            Diagonal -> foldl' addPoint m (diagHandler l)
                            where addPoint m coord = M.insertWith (<>) coord [l] m

countOverlaps :: Foldable f => f [a] -> Int
countOverlaps = length . filter ((> 1) . length) . toList

part1 :: [Line] -> Int
part1 = countOverlaps . linesToGrid mempty

part2 :: [Line] -> Int
part2 = countOverlaps . linesToGrid pointsOnDiagonal

main :: IO ()
main = do
    l <- map (splitOn (==',')) . lines <$> readFile "./inputs/5.txt"
    let lines = map (\(a:b:c:d:f) -> (Line { _from = (read a :: Int, read b :: Int), _to = (read c :: Int, read d :: Int)})) l
    print $ part2 lines
  