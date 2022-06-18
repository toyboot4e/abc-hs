#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

unreachable = error "unreachable"

sortWith :: Ord o => (a -> o) -> [a] -> [a]
sortWith = sort .comparing

main :: IO ()
main = do
  -- xs <- CM.replicateM 10 getInt
  -- CM.mapM_ $ print $ solve xs [1, 2]
  return ()

solve :: Int -> Int -> Int
solve a b = undefined
