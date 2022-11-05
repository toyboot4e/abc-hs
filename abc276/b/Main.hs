#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array

-- {{{ Imports
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import GHC.Event (IOCallback)
import GHC.Exts
import GHC.Float (int2Float)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- vector-algorithms: https://www.stackage.org/haddock/lts-16.11/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
import qualified Data.Vector.Algorithms.Intro as VAI

-- containers: https://www.stackage.org/lts-16.11/package/containers-0.6.2.1
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{- ORMOLU_ENABLE -}

-- }}}

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- safe list access
-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead (a : as) = Just a
--
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail (a : as) = Just as

-- sortWith
--
-- sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
-- sortWithDesc = sortBy . flip . comparing
--
-- maximumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- maximumWith = maximumBy . comparing
--
-- minimumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- minimumWith = minimumBy . comparing

-- }}}

-- {{{ Template

-- compress duduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress [] = []
compress (x : xs) = x : (compress $ dropWhile (== x) xs)

-- e.g. binary ocombinations:
-- combination 2 [0..8]
combinations :: Int -> [a] -> [[a]]
combinations n xs = comb n (length xs) xs
  where
    comb 0 _ _ = [[]]
    comb r n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange i j = VU.enumFromN i (j + 1 - i)

-- | Binary search for a sorted items which returns the `(ok, ng)` index pair at the boundary.
-- |
-- | Example
-- | -------
-- |
-- | With an OK predicate `(<= 5)`, list `[0..9]` can be seen as:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | In this case `bsearchMain` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearchMain (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
-- |
-- | Note that the closure CANNOT BE an exact match. Use a range-based predicate instead.
-- |
-- | Errors in edge cases
-- | --------------------
-- |
-- | While this function works for the above example, be warned that it's incomplete.
-- | It makes errors in certain edge cases:
-- |
-- | 1. The only `ok` item is at the end of the list.
-- | 2. The only `ng` item is at the beginning of the list.
-- | 3. There's no `ok` item or there's no `ng` item.
-- |
-- | So this function is wrapped by `bsearch`.
bsearchMain :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
bsearchMain (ok, ng) isOk
  | abs (ok - ng) == 1 = (ok, ng)
  | isOk m = bsearchMain (m, ng) isOk
  | otherwise = bsearchMain (ok, m) isOk
  where
    m = (ok + ng) `div` 2

-- | Binary search for inclusive range (from left to right only)
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, top) isOk = bimap wrap wrap result
  where
    result = bsearchMain (low - 1, top + 1) isOk'

    isOk' :: Int -> Bool
    isOk' x
      | x == low - 1 = True
      | x == top + 1 = False
      | otherwise = isOk x

    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == top + 1 = Nothing
      | otherwise = Just x

-- }}}

main :: IO ()
main = do
  [n, m] <- getLineIntList
  -- city (a, b) connected
  abs <- replicateM m getLineIntList
  let abs' = concatMap (\[a, b] -> [(a, b), (b, a)]) $ abs
  let array = accumArray (\acc city -> city : acc) [] (0, n) abs'
  forM_ [1..n] $ \i -> do
    let links = sort $ array ! i
    putStrLn . unwords $ (show . length $ links) : (map show links)
  return ()

