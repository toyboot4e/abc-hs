#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector

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
import Text.Printf

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
  [a, b, d] <- getLineInts
  let [x, y] = solve a b d
  printf "%f %f" x y

solve :: Int -> Int -> Int -> [Double]
solve a b d = map (len *) [cos $ arg + d', sin $ arg + d']
  where
    int2Double = fromIntegral :: Int -> Double
    x = int2Double a
    y = int2Double b
    d' = (int2Double d) / 180.0 * pi
    arg = atan2 y x
    len = sqrt $ x * x + y * y
