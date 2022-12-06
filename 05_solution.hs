import System.IO
import Control.Monad
import Data.Text (strip)
import Data.Char (isSpace)
import Data.List.Split (splitOn)

data Move = Move Int Int Int deriving Show

main = do
	input <- readFile "05_input.txt"
	let (stack, moves) = parseInput (lines input)

	-- part 1
	print $ foldr (\x y -> head x : y) "" $ foldl applyMove stack moves

	-- part 2
	print $ foldr (\x y -> head x : y) "" $ foldl applyMove9001 stack moves


transposeString ([]:_) = []
transposeString x = map head x : transposeString (map tail x)

firstNonSpace :: String -> Char
firstNonSpace (c:s)
	| c /= ' ' = c
	| otherwise = firstNonSpace s

parseInput :: [String] -> ([String], [Move])
parseInput inp = (parseStack (take (length splS - 1) splS) , map parseMove splM)
	where
		splS:splM:_ = splitOn [""] inp

parseStack :: [String] -> [String]
parseStack xs = map (dropWhile isSpace) $ transposeString $ map (map snd . filter ((==1) . fst) . zip (cycle [0..3])) xs

parseMove :: String -> Move
parseMove s = Move (read $ ss !! 1) (read $ ss !! 3) (read $ ss !! 5)
	where
		ss = splitOn " " s

stackPushS :: [String] -> Int -> String -> [String]
stackPushS st i s = take i st ++ [s ++ (st !! i)] ++ drop (i + 1) st

stackPopN :: [String] -> Int -> Int -> [String]
stackPopN st i n = take i st ++ [drop n (st !! i)] ++ drop (i + 1) st

applyMove :: [String] -> Move -> [String]
applyMove xs (Move a b c) = stackPopN (stackPushS xs (c - 1) s) (b - 1) (length s)
	where
		s = reverse $ take a (xs !! (b - 1))

applyMove9001 :: [String] -> Move -> [String]
applyMove9001 xs (Move a b c) = stackPopN (stackPushS xs (c - 1) s) (b - 1) (length s)
	where
		s = take a (xs !! (b - 1))

