#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Arrow
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

-- FIXME:
-- getLineN :: Int -> IO Vector Int
-- getLineN = VU.unfoldrN n BS.readInt <$> BS.getLine

-- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readInt <$> B.getLine

-- VU.replicateM m getLineN n
-- VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readInt <$> B.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

-- TODO: faster
getLineInts2 :: IO (Int, Int)
getLineInts2 = do
  xs <- bsToInts <$> BS.getLine :: IO [Int]
  return (xs !! 0, xs !! 1)

main :: IO ()
main = do
  n <- getLineInt
  rs <- replicateM n getLineInts2
  let res = solve rs
      bs = map (intTupleToBsb $ BSB.char7 ' ') res
   in BSB.hPutBuilder stdout $
        (mconcat $ intersperse (BSB.char7 '\n') bs) <> (BSB.char7 '\n')

intTupleToBsb :: BSB.Builder -> (Int, Int) -> BSB.Builder
-- TODO: use tuple
intTupleToBsb c xs = mconcat $ intersperse c $ [BSB.intDec $ fst xs, BSB.intDec $ snd xs]

solve :: [(Int, Int)] -> [(Int, Int)]
solve rs =
  let rs' = sortBy (\(!l, !_) (!l', !_) -> compare l l') rs
   in foldl mergeMany [head rs'] (tail rs')

mergeMany :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mergeMany rs r1@(!_, !_) = concat $ map (\ !r0 -> merge r0 r1) rs

merge :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
merge r0@(!l, !r) r1@(!l', !r') =
  if
      | l <= l' && l' <= r -> [(l, r `max` r')]
      | l' <= l && l <= r' -> [(l', r `max` r')]
      | otherwise -> [r0, r1]
