#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers
--package hashable --package unordered-containers --package heaps
--package vector --package vector-algorithms --package primitive --package transformers
-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- {{{ Imports

module Main (main) where

import Control.Monad
import Data.Array.IArray
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.IntSet as IS
import Data.List

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

type Graph = Array Int [Int]

dfsEveryPath :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph -> Int -> s -> s
dfsEveryPath (isEnd, fin, fout) graph start s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> s
    visitNode (s, visits) x
      | isEnd s = s
      | otherwise = flip fout x $ visitNeighbors (fin s x, IS.insert x visits) x

    visitNeighbors :: (s, IS.IntSet) -> Int -> s
    visitNeighbors (s, visits) x
      | isEnd s = s
      | otherwise =
        foldl' (\s2 n -> visitNode (s2, visits) n) s $ filter (`IS.notMember` visits) (graph ! x)

main :: IO ()
main = do
  [n, m] <- getLineIntList
  input <- concatMap (\[u, v] -> [(u, v), (v, u)]) <$> replicateM m getLineIntList
  let graph = accumArray @Array (flip (:)) [] (1, n) input

  let nPaths = dfsEveryPath ((== 1_000_000), const . succ, const) graph 1 (0 :: Int)
  print nPaths
