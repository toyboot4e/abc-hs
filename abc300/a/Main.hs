#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  xs <- map read . words <$> getLine

  let i = succ . fromJust $ findIndex (== (a + b)) xs
  print i
