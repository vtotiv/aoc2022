import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main = do
	input <- readFile "01_input.txt"
	-- part 1
	print $ maximum $ map (sum . map read) (splitWhen null (lines input))

	-- part 2
	print $ sum $ take 3 (sortBy (flip compare) (map (sum . map read) (splitWhen null (lines input))))
