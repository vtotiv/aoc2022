{-# LANGUAGE ViewPatterns #-}
import System.IO
import Control.Monad
import Data.List (stripPrefix, sortBy)
import Data.List.Split (splitOn)
import Debug.Trace
import Data.Map (Map, empty, insertWith, foldr, filter, toList)
import qualified Data.Map as Map

main = do
	input <- readFile "07_input.txt"

	-- part 1
	print $ Map.foldr (addIfr (<=100000)) 0 $ snd $ foldl parseCommand ([], empty) $ lines input

	-- part 2
	let l = sortBy (flip compare) $ map snd $ Map.toList $ snd $ toRoot $ foldl parseCommand ([], empty) $ lines input
	print $ last $ Prelude.filter (>(30000000 - (70000000 - (head l)))) l

addIfr :: (Int -> Bool) -> Int -> Int -> Int
addIfr f x y
	| f x = x + y
	| otherwise = y

-- takes current path as list and Map of all directory sizes
-- a comman
-- and returns the updated path and Map
parseCommand :: ([String], Map Int Int) -> String -> ([String], Map Int Int)
parseCommand (path, m) (stripPrefix "$ cd /" -> Just str)= ([], m)
parseCommand (path, m) (stripPrefix "$ cd .." -> Just str)= (tail path, insertWith (+) (key $ tail path) (Map.findWithDefault 0 (key path) m) m)
parseCommand (path, m) (stripPrefix "$ cd " -> Just str) = (str:path, m)
parseCommand (path, m) (stripPrefix "dir " -> Just str) = (path, m)
parseCommand (path, m) (stripPrefix "$ ls" -> Just str) = (path, m)
parseCommand (path, m) str = (path, insertWith (+) (key path) (size str) m)

key :: [String] -> Int
key [] = 0
key path = hash $ foldl1 (++) path

size :: String -> Int
size "" = 0
size s = read $ head $ splitOn " " s

hash :: String -> Int
hash = Prelude.foldr (\c acc -> acc * 31 + ord c) 0
	where ord c = fromEnum c

toRoot :: ([String], Map Int Int) -> ([String], Map Int Int)
toRoot (path, m) = foldl (\(p, map) s -> parseCommand (p, map) "$ cd ..") (path, m) path
