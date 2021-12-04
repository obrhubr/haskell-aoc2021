import Data.List

parseL :: [String] -> [(String, Int)]
parseL i = zip (map (head . words) i) (map ((read :: String -> Int) . (!! 1) . words) i)

part2 :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
part2 [] (_, _, _) = (0, 0, 0)
part2 (command:cs) (forward, depth, aim)
  | null cs = pos
  | otherwise = part2 cs pos
    where pos = (forward + fs, depth + as, aim + us + ds)
          fs = if cmd == "forward" then num else 0
          us = if cmd == "down" then num else 0
          ds = if cmd == "up" then -num else 0
          as = if cmd == "forward" then aim * num else 0
          (cmd, num) = command

part1 :: [(String, Int)] -> (Int, Int) -> (Int, Int)
part1 [] (_, _) = (0, 0)
part1 (command:cs) (forward, depth)
  | null cs = newpos
  | otherwise = part1 cs newpos
    where newpos = (forward + fs, depth + ds + us)
          fs = if cmd == "forward" then num else 0
          us = if cmd == "down" then num else 0
          ds = if cmd == "up" then -num else 0
          (cmd, num) = command

main :: IO ()
main = do
  entries <- lines <$> readFile "./inputs/2.txt"
  print $ part2 (parseL entries) (0, 0, 0)
  