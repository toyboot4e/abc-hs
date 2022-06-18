#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

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

unreachable = error "unreachable"

sortWith :: Ord o => (a -> o) -> a -> a
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
  rs <- CM.replicateM n getLineInts
  print $ solve rs

solve :: [[Int]] -> [[Int]]
solve rs =
  -- TODO: sortWith for non-non-empty
  let rs' = sortBy (\[l, _] [l', _] -> compare l l') rs
   in -- FIXME: initial value
         concat $ foldr [rs' !! 0] mergeMany rs'

mergeMany :: [[Int]] -> [Int] -> [[Int]]
mergeMany rs r1 = concat $ map (\r0 -> merge r0 r1) rs

merge :: [Int] -> [Int] -> [[Int]]
merge r0@[l, r] r1@[l', r'] =
  if
      | r <= l' -> [[l, r `max` r']]
      | r0 == r1 -> [r0] -- FIXME:
      | otherwise -> [r0, r1]
merge _ _ = unreachable
