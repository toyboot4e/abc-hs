#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

import Control.Monad
import Data.Array.Unboxed
import Data.Tuple.Extra (both)

-- | 1 行読んで `[Int]` を作る。
getInts :: IO [Int]
getInts = map read . words <$> getLine

-- | h 行読んで `UArray (Int, Int) Char` を作る。
getMat :: Int -> Int -> IO (UArray (Int, Int) Char)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

-- | 真なら `Yes`, 偽なら `No` を返す。
yn :: Bool -> String
yn True = "Yes"
yn False = "No"

-- | 行列中のすべての添字 (y, x) に (dy, dx) を加算する。行列をはみ出た添字はループさせる。
shiftMat :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Char
shiftMat mat (dy, dx) = ixmap (bounds mat) f mat
  where
    f (y, x) = ((y + dy) `mod` h, (x + dx) `mod` w)
    (h, w) = both succ . snd $ bounds mat

main :: IO ()
main = do
  [h, w] <- getInts
  matA <- getMat h w
  matB <- getMat h w

  let dyxs = [(dy, dx) | dy <- [0 .. h - 1], dx <- [0 .. w - 1]]
  putStrLn $ yn . any (== matB) $ map (shiftMat matA) dyxs
