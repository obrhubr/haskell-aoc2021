import Data.List
import Data.Maybe (mapMaybe, isNothing)

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing c = c

scoreErrors :: Maybe Char -> Maybe Int
scoreErrors (Just ')') = Just 3
scoreErrors (Just ']') = Just 57
scoreErrors (Just '}') = Just 1197
scoreErrors (Just '>') = Just 25137
scoreErrors Nothing = Nothing
scoreErrors (Just c) = Nothing

scoreComplete :: Char -> Int
scoreComplete ')' = 1
scoreComplete ']' = 2
scoreComplete '}' = 3
scoreComplete '>' = 4
scoreComplete c = 0

totalScoreComplete :: Int -> [Char]  -> Int
totalScoreComplete t [] = t
totalScoreComplete t [c] = 5 * t + scoreComplete c
totalScoreComplete t (c:cs) = totalScoreComplete (5 * t + scoreComplete c) cs

lookForErrors :: String -> [Char] -> Maybe Char
lookForErrors _ [] = Nothing
lookForErrors st (x:xs)
    | x `elem` ['(', '[', '{', '<'] = lookForErrors (closing x : st) xs
    | x == head st = lookForErrors (tail st) xs
    | otherwise = Just x

lookForComplete :: String -> [Char] -> Maybe [Char]
lookForComplete st [] = Just st
lookForComplete st (x:xs)
    | x `elem` ['(', '[', '{', '<'] = lookForComplete (closing x : st) xs
    | x == head st = lookForComplete (tail st) xs
    | otherwise = Nothing

part1 :: [String] -> Int
part1 lines = sum $ mapMaybe (scoreErrors . lookForErrors []) lines

part2 :: [String] -> Int
part2 l =  sort scores !! (length scores `div` 2)
    where   lines = mapMaybe (\l -> if isNothing (lookForErrors [] l) then Just l else Nothing) l
            completed = mapMaybe (lookForComplete []) lines
            scores = map (totalScoreComplete 0) completed

main :: IO ()
main = do
    l <- lines <$> readFile "./inputs/10.txt"
    print $ part2 l