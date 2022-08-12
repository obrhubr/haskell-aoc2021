import Data.List
import Data.Map as M ( fromList, Map, mapWithKey, (!?), (!), mapMaybe, toList, size, filterWithKey, insert, withoutKeys, fromListWith, empty, insertWith )
import Data.Map.Strict as Mstrict ( insertWith )
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, empty, Set )

data GameState = GameState {
  pos :: (Int, Int),
  score :: (Int, Int),
  dice :: Int,
  turn :: Int
} deriving (Show)

rollDice :: GameState -> GameState
rollDice (GameState (p1, p2) (s1, s2) dice turn)
  | turn == 0 = (GameState (np1, p2) (s1 + np1, s2) (dice + 3) 1)
  | turn == 1 = (GameState (p1, np2) (s1, s2 + np2) (dice + 3) 0)
    where sm = sum [rollN .. rollN + 2]
          np1 = ((p1 + sm - 1) `mod` 10) + 1
          np2 = ((p2 + sm - 1) `mod` 10) + 1
          rollN = ((dice - 1) `mod` 100) + 1

rollUntil1000 :: GameState -> GameState
rollUntil1000 (GameState (p1, p2) (s1, s2) dice turn)
  | s1 >= 1000 = GameState (p1, p2) (s1, s2) (dice - 1) turn
  | s2 >= 1000 = GameState (p1, p2) (s1, s2) (dice - 1) turn
  | otherwise = rollUntil1000 (rollDice (GameState (p1, p2) (s1, s2) dice turn))

part1 :: GameState -> Int
part1 state
  | s1 >= 1000 = s2 *dice
  | s2 >= 1000 = s1 * dice
  | otherwise = error "impossible"
    where (GameState (p1, p2) (s1, s2) dice _) = rollUntil1000 state

oneRound :: Map ((Int, Int), Int) Int -> Map ((Int, Int), Int) Int
oneRound m = M.fromList 

part2 m = 0

main :: IO ()
main = do
    let sp = (9, 10)
    print $ part1 (GameState sp (0, 0) 1 0)