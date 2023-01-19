#!/usr/bin/env stack
{- stack script --resolver lts-16.11
--package array --package bytestring --package containers
--package hashable --package unordered-containers --package heaps
--package vector --package vector-algorithms --package primitive --package transformers
-}

{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.List

import qualified Data.ByteString.Char8 as BS

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

isqrt :: Int -> Int
isqrt = floor @Double . sqrt . fromIntegral

solve :: Int -> [Int] -> [Int]
solve _ [] = []
solve x (p : ps) =
  case x `divMod` p of
    (d, 0) ->
      case d `divMod` p of
        (q, 0) -> [p, q]
        (_, _) -> [isqrt d, p]
    (_, _) -> solve x ps

main :: IO ()
main = do
  [nTests] <- getLineIntList

  forM_ [1 .. nTests] $ \_ -> do
    [n] <- getLineIntList
    let pq = solve n [2 ..]
    putStrLn $ unwords (map show pq)
