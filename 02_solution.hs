import System.IO
import Control.Monad
import Data.Char (ord, chr)

main = do
	input <- readFile "02_input.txt"
	-- part 1
	print $ evalGame $ lines input

	-- part 2
	print $ evalGame $ map replace (lines input)


evalGame :: [String] -> Int
evalGame s= foldr (\x y -> ((ord (last x)) - 87 + y) + (evalWin x)) 0 s

evalWin :: String -> Int
evalWin s
	| (x == -1 || x == 2) = 6
	| (x == -2 || x == 1) = 0
	| otherwise = 3
		where
			x = (ord (head s) - 64) + ((-1) * (ord (last s) -87))

replace :: String -> String
replace s
	| (last s) == 'X' = head s : (chr $ ord (head s) `mod` 3 + 88) : []
	| (last s) == 'Y' = head s : (chr $ ord (head s) + 23) : []
	| otherwise   	  = head s : (chr $ (ord (head s) - 64) `mod` 3 + 88) : []
