import System.IO
import Control.Monad
import Data.List

main = do
	input <- readFile "01_input.txt"
	print (foldr1 (max) (map sum (map (map toInt) (map (delete "") (groupBy (\x y -> not (null y)) (lines input))))))

toInt :: String -> Int
toInt = read
