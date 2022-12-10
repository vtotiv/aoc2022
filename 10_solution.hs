{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.List (stripPrefix)

main = do
	input <- readFile "10_input.txt"

	-- part 1
	print $ stateSum $ foldl parseCommand (State 1 1 []) $ lines input

	-- part 2
	putStrLn $ crtString $ foldl parseDraw (Crt 0 1 [""]) $ lines input

data State = State Int Int [Int] deriving Show
data Crt = Crt Int Int [String] deriving Show

parseCommand :: State -> String -> State
parseCommand (State cyc reg sig) (stripPrefix "noop" -> Just str) = evalSig $ State (cyc + 1) reg sig
parseCommand (State cyc reg sig) (stripPrefix "addx " -> Just str) = parseCommand (evalSig $ State (cyc + 1) reg sig) str
parseCommand (State cyc reg sig) str = evalSig $ State (cyc + 1) (read str + reg) sig

parseDraw :: Crt -> String -> Crt
parseDraw (Crt cyc sprite rows) (stripPrefix "noop" -> Just str) = drawCrt $ Crt (cyc + 1) sprite rows
parseDraw (Crt cyc sprite rows) (stripPrefix "addx " -> Just str) = parseDraw (drawCrt $ Crt (cyc + 1) sprite rows) str
parseDraw (Crt cyc sprite rows) str = drawCrt $ Crt (cyc + 1) (read str + sprite) rows

evalSig :: State -> State
evalSig (State cyc reg sig)
	| cyc `mod` 40 == 20 = State cyc reg ((cyc * reg) : sig)
	| otherwise = State cyc reg sig

drawCrt :: Crt -> Crt
drawCrt (Crt cyc sprite rows)
	| length (head rows) == 40 = drawCrt $ Crt cyc sprite ("" : rows)
	| p >= sprite - 1 && p <= sprite + 1 = Crt cyc sprite (("#" ++ (head rows)) : tail rows)
	| otherwise = Crt cyc sprite (("." ++ (head rows)) : tail rows)
		where p = cyc `mod` 40

stateSum :: State -> Int
stateSum (State _ _ sig) = sum sig

crtString :: Crt -> String
crtString (Crt _ _ sl) = foldr (\x y -> reverse (tail x ++ [head x]) ++ "\n" ++ y) "" $ reverse sl
