import System.IO
import Control.Monad
import Data.List

main = do
	input <- readFile "01_input.txt"
	-- part 1
	print (maximum (map (sum . (map read) . (delete "")) (groupBy (\x y -> not (null y)) (lines input))))
	-- part 2
	print (sum (take 3 (sortBy (flip compare) (map (sum . (map read) . (delete "")) (groupBy (\x y -> not (null y)) (lines input))))))
