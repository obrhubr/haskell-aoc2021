import Control.Monad.State
    ( MonadState(put, get), runState, State )
import System.IO (isEOF)
import Text.Parsec ( char, digit, (<|>), parse )

data Snailfish = Leaf Int | Branch Snailfish Snailfish
    deriving (Eq)

instance Show Snailfish where
    show (Leaf n) = show n
    show (Branch a b) = "[" ++ show a ++ "," ++ show b ++ "]"

isLeaf (Leaf n) = True
isLeaf (Branch a b) = False

-- Input parser
pSnailfish = pLeaf <|> pBranch

pLeaf = do
    n <- digit
    return (Leaf (read [n]))

pBranch = do
    char '['
    left <- pSnailfish
    char ','
    right <- pSnailfish
    char ']'
    return (Branch left right)

parseLine :: String -> Snailfish
parseLine s = case parse pSnailfish "(input)" s of
    Left e -> error ("Parse error: " ++ show e)
    Right snailfish -> snailfish

-- Input/output
getLines :: IO [String]
getLines = do
    eof <- isEOF
    if eof
        then return []
        else do
            line <- getLine
            remaining_lines <- getLines
            return (line : remaining_lines)

-- Snailfish arithmetic
add a b = reduce $ Branch a b

reduce s = reduce' (s, True)

reduce' (s, continue) = if continue
    then reduce' (reduceOnce s)
    else s

reduceOnce s = let
    (exploded, done) = explode s
    in if done
        then (exploded, True)
        else split s

data ExplodeState = ExplodeState {
    leaves :: [Int],
    addend :: Int,
    done :: Bool
} deriving (Show)

explode' :: Int -> Snailfish -> State ExplodeState Snailfish
explode' depth (Leaf n) = do
    s <- get
    let a = addend s
    put s { leaves = (n + a):leaves s, addend = 0 }
    return (Leaf (n + a))
explode' depth (Branch a b) = do
    s <- get
    if not (done s) && depth > 3 && isLeaf a && isLeaf b
        then do
            let (l:ls) = leaves s
            let (Leaf va) = a
            let (Leaf vb) = b
            put s { leaves = 0:(l + va):ls, addend = vb, done = True }
            return (Leaf 0)
        else do
            ra <- explode' (depth + 1) a
            rb <- explode' (depth + 1) b
            return (Branch ra rb)

explode :: Snailfish -> (Snailfish, Bool)
explode s = let (tree, state) = runState (explode' 0 s) (ExplodeState [] 0 False)
    in (fst $ replaceLeaves tree (leaves state), done state)

replaceLeaves s [] = error "Not enough numbers"
replaceLeaves (Leaf n) (x:xs) = (Leaf x, xs)
replaceLeaves (Branch a b) xs = let
    (t, ys) = replaceLeaves b xs
    (s, zs) = replaceLeaves a ys
    in (Branch s t, zs)

split (Leaf n)
    | n > 9 = (Branch (Leaf (n `div` 2)) (Leaf (n `div` 2 + n `mod` 2)), True)
    | otherwise = (Leaf n, False)
split (Branch a b) = let
    (a', done) = split a
    in if done
        then (Branch a' b, True)
        else let
            (b', b_done) = split b
            in (Branch a' b', b_done)

magnitude (Leaf n) = toInteger n
magnitude (Branch a b) = 3 * magnitude a + 2 * magnitude b

-- Main program

main = do
    lines <- getLines
    let fishes = map parseLine lines

    print fishes

    -- Part 1
    -- print $ magnitude $ foldl1 add $ fishes

    -- Part 2
    -- print $ maximum [ magnitude (add a b) | a <- fishes, b <- fishes, a /= b ]