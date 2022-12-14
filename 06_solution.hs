import System.IO
import Control.Monad
import Data.List (nub)

main = do
	input <- readFile "06_input.txt"

	-- part 1
	print $ (+4) $ length $ takeWhile (not . isUnique) $ perms 4 input

	-- part 2
	print $ (+14) $ length $ takeWhile (not . isUnique) $ perms 14 input


perms :: Int -> String -> [String]
perms n s = filter ((==n) . length) $ map (\x -> take n (drop x s)) [0..(length s - n)]

isUnique :: String -> Bool
isUnique s = length s == length (nub s)
