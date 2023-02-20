#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package containers

-- {{{ Imports
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import GHC.Event (IOCallback)
import GHC.Float (int2Float)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- bytestring
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

-- containers
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{- ORMOLU_ENABLE -}

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- - filter_map = mapMaybe

-- }}}

-- {{{ Template

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : as) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (a : as) = Just as

sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
sortWithDesc = sortBy . flip . comparing

maximumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
maximumWith = maximumBy . comparing

minimumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
minimumWith = minimumBy . comparing

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

bsToIntList :: BS.ByteString -> [Int]
bsToIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

bsToIntVec :: BS.ByteString -> VU.Vector Int
bsToIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineIntList :: IO [Int]
getLineIntList = bsToIntList <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange i j = VU.enumFromN i (j + 1 - i)

combinations :: Int -> [a] -> [[a]]
combinations n xs = comb n (length xs) xs
  where
    comb 0 _ _ = [[]]
    comb r n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs

-- }}}

main :: IO ()
main = do
  [h, w] <- getLineIntList
  mat <- BS.concat <$> replicateM h BS.getLine

  let (x, y) = solve h (w, mat)
  if x == -1
    then putStrLn "-1"
    else printf "%d %d\n" (y + 1) (x + 1)

readMat :: (Int, BS.ByteString) -> Int -> Int -> Char
readMat (w, mat) x y = BS.index mat (x + y * w)

solve :: Int -> (Int, BS.ByteString) -> (Int, Int)
solve h (w, mat) = loop (w, mat) (0, 0) 0
  where
    loop (w, mat) (x, y) i =
      if isLooping i
        then (-1, -1)
        else
          let (x2, y2) = step (w, mat) (x, y)
           in if not (isInBound x2 y2)
                then (x, y)
                else loop (w, mat) (x2, y2) (i + 1)

    isLooping i = i >= h * w
    isInBound x y = not (x < 0 || w <= x) && not (y < 0 || h <= y)

    step (w, mat) (x, y) =
      let c = readMat (w, mat) x y
       in let (dx, dy) = delta c
           in (x + dx, y + dy)

    delta c = case c of
      'U' -> (0, -1)
      'D' -> (0, 1)
      'L' -> (-1, 0)
      'R' -> (1, 0)
