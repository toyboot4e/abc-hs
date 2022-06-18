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

bsToInts :: BS.ByteString -> [Int]
bsToInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineInts :: IO [Int]
getLineInts = bsToInts <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

main :: IO ()
main = do
  xs <- getLineInts
  let hs = take 3 xs -- row sum
  let ws = drop 3 xs -- col sum
  print $ length $ solve hs ws

-- TODO: use efficient 2D grid data type
-- TODO: use vector
solve :: [Int] -> [Int] -> [[[Int]]]
solve hs ws =
  [ [row1, row2, row3]
    | row1 <- genRows (hs !! 0),
      row2 <- genRows (hs !! 1),
      let row3 =
            [ ws !! 0 - (row1 !! 0 + row2 !! 0),
              ws !! 1 - (row1 !! 1 + row2 !! 1),
              ws !! 2 - (row1 !! 2 + row2 !! 2)
            ],
      all (> 0) row3,
      sum row3 == hs !! 2
  ]

genRows :: Int -> [[Int]]
genRows sumN =
  -- TODO: less check
  [[a, b, c] | a <- [1 .. sumN], b <- [1 .. (sumN - a)], let c = sumN - (a + b), b > 0, c > 0]
