-- aoc 2024 day 1 both parts
module Main where

import qualified Data.HashMap.Strict as M
import Data.List (sort)

parse :: FilePath -> IO ([Int], [Int])
parse path = do
    content <- readFile path
    let rows = map (map read . words) (lines content) :: [[Int]]
    let (xs, ys) = unzip [(x, y) | [x, y] <- rows]
    return (xs, ys)

-- part 1
-- sort the lists, and sum the |x-y| between list
partOne :: [Int] -> [Int] -> IO ()
partOne xs ys = do
    print $ sum [abs (x - y) | (x, y) <- zip (sort xs) (sort ys)]

-- part 2
-- we only need to convert the right list to a dictionary in terms of left list.
-- look up value and multiply directly
partTwo :: [Int] -> [Int] -> IO ()
partTwo xs ys = print $ sum [n * freq n | n <- xs]
  where
    counts = M.fromListWith (+) [(y, 1) | y <- ys]
    freq n = M.lookupDefault 0 n counts

main :: IO ()
main = do
    (xs, ys) <- parse "./input.txt"
    partOne xs ys
    partTwo xs ys
