#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

module Main (main) where

import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import GHC.Event (IOCallback)
import GHC.Float (int2Float)

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

-- TODO: Try using as input Vector as input. Make a recursive function with Vector.
bsToInts :: BS.ByteString -> [Int]
bsToInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineInts :: IO [Int]
getLineInts = bsToInts <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

main :: IO ()
main = do
  _n <- getLineInt
  xs <- getLineInts
  cs <- getLineInts
  print $ solve xs

solve :: [Int] -> Int
solve xs = undefined

solve2 :: [Int] -> [Int] -> [Int]
solve2 [i0, i1] [x0, x1] [c0, c1] =
  case (i0 == x0, i1 == x1) of
    (true, true) -> if c0 >= c1 then [c0, c1] else [c1, c0]
    (true, false) -> [c1, c0]
    (false, true) -> [c0, c1]
    -- either
    (false, false) -> [c0, c1]

solve3 :: [Int] -> [Int] -> [Int]
solve3 [i0, i1, i2] [x0, x1, x2] [c0, c1, c2] =
  case (i0 == x0, i1 == x1) of
    (true, true) -> if c0 >= c1 then [c0, c1] else [c1, c0]
    (true, false) -> [c1, c0]
    (false, true) -> [c0, c1]
    -- either
    (false, false) -> [c0, c1]
