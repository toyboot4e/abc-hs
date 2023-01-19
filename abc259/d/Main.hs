#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad
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
  [n] <- getLineInt
  [sx, sy, tx, ty] <- getLineInt
  circles <- replicateM n getLineInts
  print $ solve circles

solve :: [[Int]] -> Int
solve circles = _

circleContains :: [Int] -> [Int] -> Bool
circleContains [cx, cy, r] [x, y] = (dx * dx + dy * dy) - r * r < == 0.0
  where dx = cx - x
        dy = cy - y

canUnite :: [Int] -> [Int] -> Bool
canUnite [cx0, cy0, r0] [cx1, cy1, r1] = d >= r0 + r1
  where dx = cx0 - cx1
        dy = cy0 - cx1
        d = sqrt $ dx * dx + dy * dy
