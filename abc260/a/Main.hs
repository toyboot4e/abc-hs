#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Applicative
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
import Text.Printf

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (a:as) = Just as

main :: IO ()
main = do
  xs <- getLine
  let c = safeHead $ selectNonDups xs
  let s = fromMaybe "" (fmap (\c -> [c]) c)
  putStrLn $ if null s then "-1" else s

selectNonDups :: [Char] -> [Char]
selectNonDups xs = mapMaybe (\i -> if isDup i xs then Nothing else Just (xs !! i)) [0..2]

isDup :: Int -> [Char] -> Bool
isDup i cs = ci == cj || ci == ck
  where ci = cs !! i
        cj = cs !! j
        ck = cs !! k
        j = (i + 1) `mod` 3
        k = (i + 2) `mod` 3

