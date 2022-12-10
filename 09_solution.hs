{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.List (sort, group, stripPrefix)

type Pos = (Int, Int)
type Rope = [Pos]
-- State Head Tail Visited
data State = State Rope [Pos] deriving Show

rope1 :: Rope
rope1 = [(0, 0), (0, 0)]
rope2 :: Rope
rope2 = iterate ((0, 0):) [] !! 10

main = do
	input <- readFile "09_input.txt"

	-- part 1
	print $ length $ groupPos $ visited $ foldl parseMotion (State rope1 []) $ lines input

	-- part 2
	print $ length $ groupPos $ visited $ foldl parseMotion (State rope2 []) $ lines input

parseMotion :: State -> String -> State
parseMotion state (stripPrefix "R " -> Just str) = foldr (\x y -> moveHead y (1, 0)) state [0..(read str - 1)]
parseMotion state (stripPrefix "L " -> Just str) = foldr (\x y -> moveHead y (-1, 0)) state [0..(read str - 1)]
parseMotion state (stripPrefix "U " -> Just str) = foldr (\x y -> moveHead y (0, 1)) state [0..(read str - 1)]
parseMotion state (stripPrefix "D " -> Just str) = foldr (\x y -> moveHead y (0, -1)) state [0..(read str - 1)]
parseMotion state str = state

moveHead :: State -> Pos -> State
moveHead (State ((hx, hy):t) v) (mx, my) = moveTail $ State ((hx + mx, hy + my):t) v

moveTail :: State -> State
moveTail (State (h:x:[]) v) = State [h, nx] (nx:v)
	where nx = movePos h x
moveTail (State (h:x:t) v) = State (h:nr) nv
	where
		(State nr nv) = moveTail $ State (nx:t) v
		nx = movePos h x

movePos :: Pos -> Pos -> Pos
movePos (hx, hy) (tx, ty)
	| dist >= 4 = (tx + sign (hx - tx), ty + sign (hy - ty))
	| otherwise = (tx, ty)
		where
			dist = (hx - tx)^2 + (hy - ty)^2

sign :: Int -> Int
sign x
	| x > 0 = 1
	| x < 0 = -1
	| otherwise = 0

visited :: State -> [Pos]
visited (State _ l) = l

groupPos :: [Pos] -> [Pos]
groupPos = map head . group . sort

