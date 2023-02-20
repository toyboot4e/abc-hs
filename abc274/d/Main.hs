#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package vector-algorithms --package containers --package array

-- -- {{{ Imports
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE TypeApplications #-}
-- 
-- module Main (main) where
-- 
-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.Fix
-- import Control.Monad.ST
-- import Data.Array
-- import Data.Bits
-- import Data.Char
-- import Data.IORef
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import GHC.Event (IOCallback)
-- import GHC.Float (int2Float)
-- import System.IO
-- import Text.Printf
-- 
-- {- ORMOLU_DISABLE -}
-- 
-- -- bytestring: https://www.stackage.org/lts-16.31/package/bytestring-0.10.10.0
-- import qualified Data.ByteString.Builder as BSB
-- import qualified Data.ByteString.Char8 as BS
-- 
-- -- vector: https://www.stackage.org/lts-16.31/package/vector-0.12.1.2
-- import qualified Data.Vector.Fusion.Bundle as VFB
-- import qualified Data.Vector.Generic as VG
-- import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as VM
-- 
-- -- vector-algorithms: https://www.stackage.org/haddock/lts-16.31/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
-- import qualified Data.Vector.Algorithms.Intro as VAI
-- import qualified Data.Vector.Algorithms.Intro as VAI
-- 
-- -- containers: https://www.stackage.org/lts-16.31/package/containers-0.6.2.1
-- import qualified Data.IntMap.Strict as IM
-- import qualified Data.IntSet as IS
-- import qualified Data.Map.Strict as M
-- 
-- {- ORMOLU_ENABLE -}
-- 
-- -- Option - Maybe cheatsheet
-- -- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe
-- 
-- -- }}}
-- 
-- -- {{{ Template
-- 
-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead (a : as) = Just a
-- 
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail (a : as) = Just as
-- 
-- sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
-- sortWithDesc = sortBy . flip . comparing
-- 
-- maximumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- maximumWith = maximumBy . comparing
-- 
-- minimumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- minimumWith = minimumBy . comparing
-- 
-- getLineInt :: IO Int
-- getLineInt = fst . fromJust . BS.readInt <$> BS.getLine
-- 
-- bsToIntList :: BS.ByteString -> [Int]
-- bsToIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)
-- 
-- bsToIntVec :: BS.ByteString -> VU.Vector Int
-- bsToIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace)
-- 
-- getLineIntList :: IO [Int]
-- getLineIntList = bsToIntList <$> BS.getLine
-- 
-- getLineIntVec :: IO (VU.Vector Int)
-- getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
-- 
-- {-# INLINE vLength #-}
-- vLength :: (VG.Vector v e) => v e -> Int
-- vLength = VFB.length . VG.stream
-- 
-- {-# INLINE vRange #-}
-- vRange :: Int -> Int -> VU.Vector Int
-- vRange i j = VU.enumFromN i (j + 1 - i)
-- 
-- combinations :: Int -> [a] -> [[a]]
-- combinations n xs = comb n (length xs) xs
--   where
--     comb 0 _ _ = [[]]
--     comb r n a@(x : xs)
--       | n == r = [a]
--       | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
-- 
-- bsearch :: (Int -> Bool) -> Int -> Int -> Int
-- bsearch match l u
--   | match l = l
--   | otherwise = loop l u
--   where
--     loop x y
--       | y == succ x = y
--       | match z = z
--       | otherwise = loop z y
--       where
--         z = (x + y) `div` 2
-- 
-- -- bsearch :: (Int -> Bool) -> Int -> Int -> Int
-- -- bsearch inRightArea l u | inRightArea l = l
-- --                         | otherwise     = loop l u
-- --     where
-- --           loop x y | y == succ x = y
-- --                    | inRightArea z = loop x z
-- --                    | otherwise     = loop z y
-- --               where z = (x + y) `div` 2
-- 
-- -- compress duduplicates sorted list, nub deduplicates non-sorted list
-- -- TODO: std?
-- compress [] = []
-- compress (x : xs) = x : (compress $ dropWhile (== x) xs)
-- 
-- -- }}}
-- 
-- -- https://stackoverflow.com/questions/3707753/haskell-split-even-and-odd-elements-into-tuple
-- uninterleave :: [a] -> ([a], [a])
-- uninterleave [] = ([], [])
-- uninterleave [x] = ([x], [])
-- uninterleave (x : y : xs) = (x : xp, y : yp) where (xp, yp) = uninterleave xs
-- 
-- -- IntSet answer:
-- -- main :: IO ()
-- -- main = do
-- --   [n, x, y] <- getLineIntList
-- -- 
-- --   (dx : dxs, dys) <- uninterleave <$> getLineIntList
-- --   let (xofs, xs) = step dx (IS.singleton 0) dxs
-- --   let (yofs, ys) = step 0 (IS.singleton 0) dys
-- --   let res = (x - xofs) `IS.member` xs && (y - yofs) `IS.member` ys
-- -- 
-- --   putStrLn $ if res then "Yes" else "No"
-- -- 
-- -- step :: Int -> IS.IntSet -> [Int] -> (Int, IS.IntSet)
-- -- step ofs set [] = (ofs, set)
-- -- step ofs set (d : ds) = step ofs' set' ds
-- --   where
-- --     ofs' = ofs + d
-- --     set' = set `IS.union` (IS.map (\x -> x - 2 * d) set)
-- 
-- -- TODO: why faster (N^2), not still 2^N? (or not yet fast enough?)
-- -- TODO: extract `ds` with foldl
-- 
-- -- vector answer
-- main :: IO ()
-- main = do
--   [n, x, y] <- getLineIntList
-- 
--   (dx : dxs, dys) <- uninterleave <$> getLineIntList
--   let (xofs, xs) = step dx (VU.fromList [0]) dxs
--   let (yofs, ys) = step 0 (VU.fromList [0]) dys
--   let res = (x - xofs) `VU.elem` xs && (y - yofs) `VU.elem` ys
-- 
--   putStrLn $ if res then "Yes" else "No"
-- 
-- step :: Int -> VU.Vector Int -> [Int] -> (Int, VU.Vector Int)
-- step ofs vec [] = (ofs, vec)
-- step ofs vec (d : ds) = step ofs' vec' ds
--   where
--     ofs' = ofs + d
--     vec' = vec VU.++ (VU.map (\x -> x - 2 * d) vec)

import qualified Data.Vector.Unboxed as VU

getLineIntList :: IO [Int]
getLineIntList = map read . words <$> getLine

main :: IO ()
main = do
  [n, x, y] <- getLineIntList
  as <- getLineIntList
  let answer = abc274d n x y as
  putStrLn $ if answer then "Yes" else "No"

abc274d :: Int -> Int -> Int -> [Int] -> Bool
abc274d n x y (a1:as) = VU.elem (x - ox) xs && VU.elem (y - oy) ys
  where
    dys = [a | (a, True) <- zip as $ cycle [True, False]]
    dxs = [a | (a, True) <- zip as $ cycle [False, True]]
    (ox,xs) = foldl step (a1, VU.singleton 0) dxs
    (oy,ys) = foldl step ( 0, VU.singleton 0) dys
    step (ofs, ns) x = (ofs + x, (VU.++) ns $ VU.map (\n -> n - x - x) ns)

