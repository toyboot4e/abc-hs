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

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

bsToInts :: BS.ByteString -> [Int]
bsToInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineInts :: IO [Int]
getLineInts = bsToInts <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

-- }}}

main :: IO ()
main = do
  !n <- getLineInt
  !as <- getLineIntVec
  -- print $ solve n as
  print $ solve2 n as

-- TLE
solve :: Int -> VU.Vector Int -> Int
solve n !as = length list
  where
    -- TODO: use vector
    list = [ () | !i <- [0..n-2],
                      !j <- [i+1..n-1],
                      let !x = as VU.! i,
                      let !y = as VU.! j,
                      x == (i+1) && y == (j+1) || x == (j+1) && y == (i+1)
             ]

solve2 :: Int -> VU.Vector Int -> Int
solve2 n !as = VU.sum counts
  where
    counts = VU.map f $ VU.enumFromN (0 :: Int) (n - 1)

    f !i = if (as VU.! i) == (i + 1) then
        vLength $ VU.filter id $ VU.map fj $ VU.enumFromN ((i+1) :: Int) (n-(i+1))
      else
        let j = (as VU.! i) - 1
            other = (as VU.! j)
        in if j > i && other == (i + 1) then 1 else 0

    fj j = as VU.! j == (j+1)
