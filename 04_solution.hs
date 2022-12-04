import System.IO
import Control.Monad
import Data.Char (ord)
import Data.List.Split

main = do
	input <- readFile "04_input.txt"
	-- part 1
	print $ length $ filter evalContains (map toInt (lines input))

	-- part 2
	print $ length $ filter evalOverlap (map toInt (lines input))

toInt :: String -> [Int]
toInt s =
	[(read a), (read b), (read c), (read d)]
	where
		(x:y:_) = splitOn "," s
		(a:b:_) = splitOn "-" x
		(c:d:_) = splitOn "-" y

evalContains :: [Int] -> Bool
evalContains (a:b:c:d:_) = ((a <= c) && (b >= d)) || ((a >= c) && (b <= d))

evalOverlap :: [Int] -> Bool
evalOverlap (a:b:c:d:_) = not (((a < c) && (b < c)) || ((a > d) && (b > d)))
