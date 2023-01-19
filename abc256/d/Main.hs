#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import GHC.Event (IOCallback)
import GHC.Float (int2Float)
import System.IO

unreachable = error "unreachable"

sortWith :: Ord o => (a -> o) -> [a] -> [a]
sortWith = sortBy . comparing

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

bsToInts :: BS.ByteString -> [Int]
bsToInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineInts :: IO [Int]
getLineInts = bsToInts <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

main :: IO ()
main = do
  n <- getLineInt
  rs <- replicateM n getLineInts

  let rs' = sortWith (\[l, _] -> l) rs
  let res = solve2 rs'

  let bs = map (\xy -> mconcat $ intersperse (BSB.char7 ' ') (map BSB.intDec xy)) res
  BSB.hPutBuilder stdout $
    (mconcat $ intersperse (BSB.char7 '\n') bs) <> (BSB.char7 '\n')

solve2 :: [[Int]] -> [[Int]]
solve2 [] = []
solve2 ([x, y] : rs) = loop x y rs
  where
    loop !x !y [] = [[x, y]]
    loop !x !y ([u, v] : rs)
      | u <= y = loop x (max y v) rs
      | otherwise = [x, y] : loop u v rs
