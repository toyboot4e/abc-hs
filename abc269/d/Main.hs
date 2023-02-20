#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package containers --package array

-- {{{ Imports
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import GHC.Event (IOCallback)
import GHC.Float (int2Float)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- FIXME: for Union-Find
import Data.Array.ST
import Data.Array.MArray

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

getLineIntList :: IO [Int]
getLineIntList = bsToIntList <$> BS.getLine

bsToIntVec :: BS.ByteString -> VU.Vector Int
bsToIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace)

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

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch match l u
  | match l = l
  | otherwise = loop l u
  where
    loop x y
      | y == succ x = y
      | match z = z
      | otherwise = loop z y
      where
        z = (x + y) `div` 2

-- bsearch :: (Int -> Bool) -> Int -> Int -> Int
-- bsearch inRightArea l u | inRightArea l = l
--                         | otherwise     = loop l u
--     where
--           loop x y | y == succ x = y
--                    | inRightArea z = loop x z
--                    | otherwise     = loop z y
--               where z = (x + y) `div` 2

-- }}}

main :: IO ()
main = do
  n <- getLineInt
  -- FIXME: maybe add `getLineIntTuple`
  xys' <- replicateM n getLineIntList
  let xys = map (\[x, y] -> (x, y)) xys'
  print $ solve xys

solve :: [(Int, Int)] -> Int
solve xs = runST $ do
  uf <- newUnionFind 40001
  -- FIXME: unite only when the cell is already filled
  mapM_ (\pos -> mapM_ (\nb -> unite uf pos nb) (neighbors pos)) xs
  return length $ root uf

toUfIndex :: (Int, Int) -> Int
toUfIndex x y = (x + 1000) + y + 1000

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (x − 1, y − 1)
      (x − 1, y)
      (x, y − 1)
      (x, y + 1)
      (x + 1, y)
      (x + 1, y + 1)
  ]

-- Union-Find
-- https://kseo.github.io/posts/2014-01-30-implementing-union-find-in-haskell.html
data UnionFind s = UnionFind
  { ids :: STUArray s Int Int,
    szs :: STUArray s Int Int
  }

newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n -1) [0 .. n -1]) (newArray (0, n -1) 1)

find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if (id /= i)
    then do
      gpid <- readArray (ids uf) id
      writeArray (ids uf) i gpid
      root uf id
    else return i

unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
  i <- root uf p
  j <- root uf q
  szi <- readArray (szs uf) i
  szj <- readArray (szs uf) j
  if (szi < szj)
    then do
      writeArray (ids uf) i j
      writeArray (szs uf) j (szi + szj)
    else do
      writeArray (ids uf) j i
      writeArray (szs uf) i (szj + szi)
