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
  -- red jowel level n
  -- Red n -> (Blue (n - 1), Blue (n - 2))
  [level, x, y] <- getLineInts
  print $ reduceRed (toInteger x) (toInteger y) level

-- count blue (level 1)

-- Red 2 -> Red 1 + x * Blue 2
reduceRed :: Integer -> Integer -> Int -> Integer
reduceRed !x !y !0 = 0
reduceRed !x !y !1 = 0
reduceRed !x !y !level = (reduceRed x y (level - 1)) + x * reduceBlue x y level

-- Red 2 -> Red 1 + y * Blue 1
reduceBlue :: Integer -> Integer -> Int -> Integer
reduceBlue !x !y !0 = 0
reduceBlue !x !y !1 = 1
reduceBlue !x !y !level = (reduceRed x y (level - 1)) + y * reduceBlue x y (level - 1)
