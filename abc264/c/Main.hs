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

-- }}}

main :: IO ()
main = do
  [h1, w1] <- getLineIntList
  !mat1' <- replicateM h1 getLineIntVec
  let !mat1 = VU.concat mat1'

  [h2, w2] <- getLineIntList
  !mat2' <- replicateM h2 getLineIntVec
  let !mat2 = VU.concat mat2'

  -- FIXME: n^2
  let !isOk = any id [ True | !rowSparse <- allSparse h2 h1,
                           !colSparse <- allSparse w2 w1,
                           sparseEq rowSparse colSparse (w1, mat1) (w2, mat2)
                       ]

  putStrLn $ if isOk then "Yes" else "No"

readMat :: Int -> Int -> (Int, VU.Vector Int) -> Int
readMat y x (w, mat) = mat VU.! (x + y * w)

allSparse :: Int -> Int -> [[(Int, Int)]]
allSparse !takeN !n = map (\xs -> zip xs [0..(takeN - 1)]) xss
  where
    xss = combinations takeN [0..(n-1)]

sparseEq :: [(Int, Int)] -> [(Int, Int)] -> (Int, VU.Vector Int) -> (Int, VU.Vector Int) -> Bool
sparseEq !rows !cols (!w1, !mat1) (!w2, !mat2) = all id $ map f data_
  where
    !data_ = [(row, col) | row <- rows, col <- cols ]
    f ((!row1, !row2), (!col1, !col2)) = (readMat row1 col1 (w1, mat1)) == (readMat row2 col2 (w2, mat2))

combinations :: Int -> [a] -> [[a]]
combinations n xs = comb n (length xs) xs where
  comb 0 _ _ = [[]]
  comb r n a@(x:xs)
    | n == r    = [a]
    | otherwise = map (x:) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
