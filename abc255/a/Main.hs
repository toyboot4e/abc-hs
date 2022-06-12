#!/usr/bin/env runghc

module Main (main) where

main :: IO ()
main = do
  [row, col] <- map read . words <$> getLine :: IO [Int]

  -- TODO: better list construction
  -- TODO: use Vector
  as1 <- map read . words <$> getLine :: IO [Int]
  as2 <- map read . words <$> getLine :: IO [Int]
  let as = as1 ++ as2

  print $ solve as row col

solve :: [Int] -> Int -> Int -> Int
solve xs row' col' = xs !! (col + 2 * row)
  where row = row' - 1
        col = col' - 1

