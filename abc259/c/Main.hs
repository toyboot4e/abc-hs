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
  s <- BS.getLine
  t <- BS.getLine

  putStrLn $
    if solve s t
      then "Yes"
      else "No"

solve :: BS.ByteString -> BS.ByteString -> Bool
solve s t = (BS.groupBy (==) s) `normEq` (BS.groupBy (==) t)
  where
    normEq [] [] = True
    normEq (x : xs) (y : ys) = (normEqStep x y) && (normEq xs ys)
    normEq _ _ = False
    normEqStep x y
      | (BS.head x) /= (BS.head y) = False
      | lx == 1 = ly == 1
      | otherwise = lx <= ly
      where
        lx = BS.length x
        ly = BS.length y
