#!/usr/bin/env stack

{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Array.IArray
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Graph as G
import Data.List

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [nVerts, nEdges] <- getLineIntList
  input <- concatMap (\[u, v] -> [(u, v), (v, u)]) <$> replicateM nEdges getLineIntList
  let graph = accumArray @Array (flip (:)) [] (1, nVerts) input
  let components = G.components graph
  print $ length components
