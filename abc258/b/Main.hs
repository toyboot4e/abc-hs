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

getLineX :: IO [Int]
getLineX = map (read . pure) <$> getLine

main :: IO ()
main = do
  n <- getLineInt
  -- TODO: use Array
  -- TODO: use Vector
  grid <- replicateM n getLineX

  let results = solve n grid
  let results2 = map (\xs -> foldl (++) [] $ map show xs) results
  let res = maximum $ (map read results2 :: [Int])
  print res

-- FIXME: use BFS
solve :: Int -> [[Int]] -> [[Int]]
solve n grid = do
  -- entry points
  let (maxVal, starts) = gridAllMax n grid

  let input =
        [ (value, nextPos)
          | start <- starts,
            let nexts = next8 n [fst start, snd start],
            nextPos <- nexts,
            let value = gridGet grid (nextPos !! 0) (nextPos !! 1)
        ]
  let (maxVal, rc) = listAllMax input in rc

gridGet :: [[Int]] -> Int -> Int -> Int
gridGet grid r c = (grid !! r) !! c

-- FIXME: use listAllMax
gridAllMax :: Int -> [[Int]] -> (Int, [(Int, Int)])
gridAllMax n grid =
  findMax
    [ (value, (row, col))
      | row <- [0 .. (n - 1)],
        col <- [0 .. (n - 1)],
        let value = (grid !! row) !! col
    ]
  where
    findMax = foldl step initState
    initState = (0, [])
    step (maxVal, rcs) (val, (r, c)) =
      if
          | val > maxVal -> (maxVal, [(r, c)])
          | val == maxVal -> (maxVal, rcs ++ [(r, c)])
          | otherwise -> (maxVal, rcs)

-- Normalize the looping grid position
normalize :: Int -> [Int] -> [Int]
normalize n [r, c] = map f [r, c]
  where
    f x = (x + n) `mod` n

-- TODO: use tuples
dir8 :: [[Int]]
dir8 = [[-1, -1], [0, -1], [1, -1], [-1, 0], [1, 0], [-1, 1], [0, 1], [1, 1]]

next8 :: Int -> [Int] -> [[Int]]
next8 n [r, c] =
  map
    ( \[r2, c2] ->
        let rr = r + r2
            cc = c + c2
         in normalize n [rr, cc]
    )
    dir8

listAllMax :: [(Int, [Int])] -> (Int, [[Int]])
listAllMax xs = foldl step (0, []) xs
  where
    step (maxVal, rcs) (val, [r, c]) =
      if
          | val > maxVal -> (maxVal, [[r, c]])
          | val == maxVal -> (maxVal, rcs ++ [[r, c]])
          | otherwise -> (maxVal, rcs)
