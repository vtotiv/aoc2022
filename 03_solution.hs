import System.IO
import Control.Monad
import Data.Char (ord, chr)
import Data.List.Split

main = do
	input <- readFile "03_input.txt"
	-- part 1
	print $ foldr prio 0 (map mklist (lines input))

	-- part 2
	print $ foldr prio 0 (map mkgroup (chunksOf 3 (lines input)))

mkgroup :: [String] -> Char
mkgroup s = head [ a | a <- s !! 0, b <- s !! 1, c <- s !! 2, a == b, b == c]

mklist :: String -> Char
mklist s = head [ a | a <- take ((length s) `div` 2) s, b <- drop ((length s) `div` 2) s, a == b]

prio :: Char -> Int -> Int
prio i y
	| (c < 91) = y + c - 64 + 26
	| otherwise = y + c - 96
		where c = ord i
