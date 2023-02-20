#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers
--package hashable --package unordered-containers --package heaps
--package vector --package vector-algorithms --package primitive --package transformers
-}

main :: IO ()
main = interact $ solve . map read . words

solve :: [Int] -> String
solve [a, b] = if b `div` 2 == a then "Yes" else "No"
solve _ = error "unreachable"

