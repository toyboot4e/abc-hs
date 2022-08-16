#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package containers

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

-- {-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange i j = VU.enumFromN i (j + 1 - i)

-- }}}

main :: IO ()
main = do
  s <- getLine
  let !xs = toCharIndexes s
  print $ reduceAll xs

toCharIndexes :: [Char] -> [Int]
toCharIndexes s = map (snd . findPair) s
  where
    indexer = zip "atcoder" [0..]
    findPair c = fromJust $ find (\(ch, i) -> ch == c) indexer

cost :: (Int, Int) -> Int
cost (c, i) = abs (c - i)

reduce :: [(Int, Int)] -> Int -> [(Int, Int)]
reduce xs n = reIndexed
  where reIndexed    = map shift rest
        rest         = take (n - 1) xs ++ drop (n + 1) xs
        shift (c, i) = (shift1 c nc, shift1 i (min ni n))
        shift1 x n   = if x >= n then x - 1 else x
        (nc, ni)     = xs !! n

reduceAll :: [Int] -> Int
reduceAll !xs = loop (zip xs [0..]) 0
  where
    loop ![] !acc = acc
    loop !ci !acc = let i     = findMaxIndex ci
                        cost' = cost (ci !! i)
                        ci'   = reduce ci i
                    in
                      loop ci' (acc + cost')
    findMaxIndex xs = snd $ maximumBy (comparing fst) (zip (map cost xs) [0..])

