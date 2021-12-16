import Data.List
import Data.Map as M ( fromList, Map, filterWithKey, (!?), (!), mapMaybe, toList, lookup )
import Data.Char (digitToInt)
import Data.Sequence as Seq ( Seq, fromList )
import Data.Foldable as F (toList)
import Data.Set as Set ( fromList, insert, singleton, minView, member)
import Data.Maybe ( fromMaybe, isJust, fromJust )
import Text.ParserCombinators.ReadP ( choice, char, count, manyTill, readP_to_S, many1, ReadP )
import Data.Functor ( (<&>) )
import Numeric (readHex)

data Packet = Literal Int Int | Operator Int Int [Packet] deriving ( Show )

htb :: String -> String -> String
htb [] rest = rest
htb (a:as) rest
    | a == '0' = htb as (rest ++ "0000")
    | a == '1' = htb as (rest ++ "0001")
    | a == '2' = htb as (rest ++ "0010")
    | a == '3' = htb as (rest ++ "0011")
    | a == '4' = htb as (rest ++ "0100")
    | a == '5' = htb as (rest ++ "0101")
    | a == '6' = htb as (rest ++ "0110")
    | a == '7' = htb as (rest ++ "0111")
    | a == '8' = htb as (rest ++ "1000")
    | a == '9' = htb as (rest ++ "1001")
    | a == 'A' = htb as (rest ++ "1010")
    | a == 'B' = htb as (rest ++ "1011")
    | a == 'C' = htb as (rest ++ "1100")
    | a == 'D' = htb as (rest ++ "1101")
    | a == 'E' = htb as (rest ++ "1110")
    | otherwise = htb as (rest ++ "1111")

hexToBin :: String -> String
hexToBin x = htb x []

bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map (fromEnum . (=='1'))

dec2bin :: Int -> String
dec2bin n = if n > 0 then dec2bin (div n 2) ++ show (mod n 2) else []

f :: String -> String
f s = if mod l 4 == 0 then s else replicate (4 - mod l 4) '0' ++ s where l = length s

bit :: ReadP Char
bit = choice [char '0', char '1']

bits :: Int -> ReadP Int
bits n = count n bit <&> bin2dec

packet :: ReadP Packet
packet = do
    version <- bits 3
    typeId <- bits 3
    if typeId == 4 then do
        s1 <- manyTill (count 5 bit) (char '0')
        s2 <- count 4 bit
        return $ Literal version (bin2dec $ concat $ map tail s1 ++ [s2])
    else do
        lengthId <- bits 1
        if lengthId == 0 then do
            n <- bits 15
            packetstring <- count n bit
            let packets = fst . last $ readP_to_S (many1 packet) packetstring
            return $ Operator version typeId packets
        else do
            n <- bits 11
            packets <- count n packet
            return $ Operator version typeId packets

part1 :: Packet -> Int
part1 (Literal a _) = a
part1 (Operator a _ xs) = a + sum (map part1 xs)

part2 :: Packet -> Int
part2 (Literal _ s) = s
part2 (Operator _ t xs)
    | t == 0 = sum rest
    | t == 1 = product rest
    | t == 2 = minimum rest
    | t == 3 = maximum rest
    | t == 5 = if head rest > last rest then 1 else 0
    | t == 6 = if head rest < last rest then 1 else 0
    | otherwise = if head rest == last rest then 1 else 0
        where rest = part2 <$> xs

main :: IO ()
main = do
    l <- readFile "./inputs/16.txt"
    let bin = hexToBin l
    let p = fst $ head $ readP_to_S packet bin
    print $ part2 p