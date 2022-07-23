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

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

-- }}}

main :: IO ()
main = do
  !n <- getLineInt
  !xs <- replicateM n BS.getLine
  solve n xs

solve :: Int -> [BS.ByteString] -> IO ()
solve !n !xs = do
  log <- H.new

  flip
    fix
    0
    ( \loop n ->
        if 0 == n
          then pure ()
          else do
            let !line = xs !! n
            may <- H.lookup log line
            step log line
            loop n + 1
    )

step :: HashTable BS.ByteString Int -> BS.ByteString -> Maybe Int -> IO ()
step !log !line (Just !x) = do
  BSB.hPutBuilder stdout $ (BSB.byteString line) <> (BSB.char7 '(') <> (BSB.intDec x) <> (BSB.char7 ')')
  H.mutate log line (\(Just i) -> Just $ i + 1)
step !log !line Nothing = do
  BS.putStrLn line
  H.insert log line 0

-- foo :: IO (HashTable BS.Char7 Int) ->

-- foo :: [Int] -> IO Int
-- foo = do
--     H.insert ht 1 1
--     return ht
