import Data.List (transpose)
import System.Random

data Square = Blank
    | One | Two | Three | Four | Five | Six | Seven | Eight
    deriving (Enum, Eq)

data Direction = GoUp | GoDown | GoLeft | GoRight
    deriving (Bounded, Enum)

strToDirection :: String -> Maybe Direction
strToDirection str
    | str == "u" = Just GoUp
    | str == "d" = Just GoDown
    | str == "l" = Just GoLeft
    | str == "r" = Just GoRight
    | otherwise  = Nothing

instance Random Direction where
    random = randomR (minBound, maxBound)
    randomR (l, u) g = (\ (x, g') -> ((toEnum x), g')) $ randomR (fromEnum l, fromEnum u) g

showSquare :: Square -> String
showSquare Blank = " "
showSquare One = "1"
showSquare x = [succ . head . showSquare $ pred x]

instance Show Square where
    show = showSquare

showPuzzle = unlines . map show

type Row = [Square]
type Puzzle = [Row]

finished = [[One, Two, Three], [Four, Five, Six], [Seven, Eight, Blank]]

isFinished :: Puzzle -> Bool
isFinished = (== finished)

isLeftEdge :: Puzzle -> Bool
isLeftEdge x = any (==Blank) (map head x)

maybeMove :: Puzzle -> Maybe Direction -> Puzzle
maybeMove x (Just y) = move x y
maybeMove x _ = x

move :: Puzzle -> Direction -> Puzzle
move x GoUp = moveUp x
move x GoDown = moveDown x
move x GoLeft = moveLeft x
move x GoRight = moveRight x

moveLeft :: Puzzle -> Puzzle
moveLeft = map moveRowLeft

moveRowLeft :: Row -> Row
moveRowLeft [x, Blank, y] = [Blank, x, y]
moveRowLeft [x, y, Blank] = [x, Blank, y]
moveRowLeft r = r

moveUp :: Puzzle -> Puzzle
moveUp = transpose . moveLeft . transpose

moveRight :: Puzzle -> Puzzle
moveRight = map reverse . moveLeft . map reverse

moveDown :: Puzzle -> Puzzle
moveDown = transpose . moveRight . transpose

main = do
    gen <- getStdGen
    let initial = foldl move finished (take 10000 (randoms gen))
    play initial

play puzzle = do
    putStrLn $ showPuzzle puzzle
    if (isFinished puzzle) then
        putStrLn "You win!"
    else do
        putStr "Next move? "
        next <- getLine
        play (maybeMove puzzle (strToDirection next))
