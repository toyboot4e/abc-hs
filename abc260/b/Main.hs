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

sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
sortWithDesc = sortBy . flip . comparing

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
  -- n:
  --   x: math
  --   y: eng
  --   z: total
  [n, x, y, z] <- getLineInts

  !math <- getLineInts
  !eng <- getLineInts

  let !is = [0..(n-1)]

  let getI !xs = map (\(i, _) -> i) xs

  let !xs = getI $ markX is math eng
  let !ix = take x xs
  let !is = sort $ drop x xs

  let !ys = getI $ markY is math eng
  let !iy = take y ys
  let !is = sort $ drop y ys

  let !zs = getI $ markZ is math eng
  let !iz = take z zs
  -- let !is = sort $ drop z zs

  -- print xs
  -- print ys
  -- print zs

  let !is = sort $ ix ++ iy ++ iz
  forM_ is $ \i -> print (i + 1)

markX :: [Int] -> [Int] -> [Int] -> [(Int, Int)]
markX !is !math !eng =
  sortWithDesc (\(_, mark) -> mark) $
    map (\i -> (i, math !! i)) is

markY :: [Int] -> [Int] -> [Int] -> [(Int, Int)]
markY !is !math !eng =
  sortWithDesc (\(_, mark) -> mark) $
    map (\i -> (i, eng !! i)) is

markZ :: [Int] -> [Int] -> [Int] -> [(Int, Int)]
markZ !is !math !eng =
  sortWithDesc (\(_, mark) -> mark) $
    map (\i -> (i, (math !! i + eng !! i))) is
