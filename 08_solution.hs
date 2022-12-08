import System.IO
import Control.Monad
import Data.List (stripPrefix, sortBy)
-- '/'
main = do
	input <- readFile "08_input.txt"

	-- part 1
	print $ foldr ((\x y -> y + foldr (\(a, b) c -> a + c) 0 x) .(bistencil '/' . reverse . bistencil '/')) 0 $ transpose $ map (bistencil '/' . reverse . bistencil '/' . zip (repeat 0)) $ lines input

	-- part 2

bistencil :: Char -> [(Int, Char)] -> [(Int, Char)]
bistencil acc [] = []
bistencil acc ((a, b):xs)
	| b > acc = (1, b) : bistencil b xs
	| otherwise = (a, b) : bistencil acc xs

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

