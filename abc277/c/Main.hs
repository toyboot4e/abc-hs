#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package bytestring --package vector --package vector-algorithms --package containers --package primitive
--package array -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- {{{ Imports

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
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

-- vector-algorithms: https://www.stackage.org/haddock/lts-16.31/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS

-- containers: https://www.stackage.org/lts-16.11/package/containers-0.6.2.1
import qualified Data.IntMap.Strict as IM
-- import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS -- TODO: is it strict?

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

-- {{{ Binary search

-- | Binary search for sorted items in an inclusive range (from left to right only)
-- |
-- | It returns the `(ok, ng)` index pair at the boundary.
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
-- | In this case `bsearch` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearch (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, high) isOk = bimap wrap wrap (loop (low - 1, high + 1))
  where
    loop (ok, ng)
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = loop (m, ng)
      | otherwise = loop (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == high + 1 = Nothing
      | otherwise = Just x

-- }}}

-- {{{ Union-Find

-- | Union-find implementation (originally by `@pel`)
newtype UnionFind s = UnionFind (VM.MVector s UfNode)

type IOUnionFind = UnionFind RealWorld

type STUnionFind s = UnionFind s

-- | `Child parent | Root size`. Not `Unbox` :(
data UfNode = Child {-# UNPACK #-} !Int | Root {-# UNPACK #-} !Int

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newUF #-}
newUF :: (PrimMonad m) => Int -> m (UnionFind (PrimState m))
newUF n = UnionFind <$> VM.replicate n (Root 1)

-- | Returns the root node index.
{-# INLINE rootUF #-}
rootUF :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
rootUF uf@(UnionFind vec) i = do
  node <- VM.read vec i
  case node of
    Root _ -> return i
    Child p -> do
      r <- rootUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VM.write vec i (Child r)
      return r

-- | Checks if the two nodes are under the same root.
{-# INLINE sameUF #-}
sameUF :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m Bool
sameUF uf x y = liftM2 (==) (rootUF uf x) (rootUF uf y)

-- | Just an internal helper.
_unwrapRoot :: UfNode -> Int
_unwrapRoot (Root s) = s
_unwrapRoot (Child _) = undefined

-- | Unites two nodes.
{-# INLINE uniteUF #-}
uniteUF :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m ()
uniteUF uf@(UnionFind vec) x y = do
  px <- rootUF uf x
  py <- rootUF uf y
  when (px /= py) $ do
    sx <- _unwrapRoot <$> VM.read vec px
    sy <- _unwrapRoot <$> VM.read vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (par, chld) = if sx < sy then (px, py) else (py, px)
    VM.write vec chld (Child par)
    VM.write vec par (Root (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE sizeUF #-}
sizeUF :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
sizeUF uf@(UnionFind vec) x = do
  px <- rootUF uf x
  _unwrapRoot <$> VM.read vec px

-- }}}

-- {{{ Misc

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

-- }}}

-- TLE (the `explored` lookup might be too slow)
-- main :: IO ()
-- main = do
--   [n] <- getLineIntList
--
--   links <-
--     IM.fromListWith (VU.++)
--       . concatMap (\[!a, !b] -> [(a, VU.singleton b), (b, VU.singleton a)])
--       <$> replicateM n getLineIntList
--
--   -- print $ links
--   print $ solve links

-- solve :: IM.IntMap (VU.Vector Int) -> Int
-- solve links = loop 1 1 IS.empty
--   where
--     loop currentMax x explored
--       | IS.member x explored = x `max` currentMax
--       | otherwise =
--         let currentMax' = x `max` currentMax
--          in case (IM.lookup x links) of
--               Nothing -> currentMax'
--               Just is ->
--                 let explored' = IS.insert x explored
--                  in VU.maximum $ VU.map (\i -> loop currentMax' i explored') is

-- [x] IntMap + breadth-first search
-- [ ] Union-find + coordinate compression (1D)
-- [ ] IntSet-based union-find

main :: IO ()
main = do
  [n] <- getLineIntList
  qs <- V.replicateM n getLineIntList

  let xs = V.uniq . V.modify VAI.sort $ V.concatMap V.fromList qs
  let nxs = V.length xs
  let indexOf x = fromJust . fst $ bsearch (0, nxs - 1) (\i -> xs V.! i <= x)

  -- TODO: less hack?
  if (xs V.! 0) /= 1
    then do putStrLn "1"
    else do
      uf <- newUF nxs

      V.forM_ qs $ \[a, b] -> do
        let ia = indexOf a
        let ib = indexOf b
        uniteUF uf ia ib

      root <-
        if (xs V.! 0) == 1
          then rootUF uf 1
          else return 1

      -- TODO: easier code?
      result <-
        fromJust
          . maximum
          . fromJust
          . sequence
          <$> mapM
            ( \i -> do
                root' <- rootUF uf i
                if root' == root
                  then return $ return $ Just (xs V.! i)
                  else return $ return Nothing
            )
            [0 .. (nxs - 1)]

      print result
