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
import Data.List (mapAccumL)
import Data.Tuple.Extra (both)

-- | 1 行読んで `[Int]` を作る。
getInts :: IO [Int]
getInts = map read . words <$> getLine

-- | h 行読んで `UArray (Int, Int) Char` を作る。
getMat :: Int -> Int -> IO (UArray (Int, Int) Char)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

-- | 1 辺の長さが `(2n + 1)` の正方形の中心座標を列挙する。
squareCenters :: Int -> (Int, Int) -> [(Int, Int)]
squareCenters n (h, w) = [(y, x) | y <- [n .. (h - 1 - n)], x <- [n .. (w - 1 - n)]]

-- | (y0, x0) を中心座標とする正方形に、大きさ n の x 印が存在するかを答える。
isCrossMark :: UArray (Int, Int) Char -> Int -> (Int, Int) -> Bool
isCrossMark mat n (y0, x0) = all isSharp crossPoints
  where
    -- その座標が `#` であるか
    isSharp (y, x) = mat ! (y, x) == '#'
    -- x 印を構成する (y, x) 座標一覧
    crossPoints = map (add2 (y0, x0)) dyxs
      where
        dyxs = [(dy, dx) | dy <- [-n .. n], dx <- [-n .. n], abs dy == abs dx]
        add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

-- | `n` に対応する x 印の数を数える。
-- | このとき n より大きな x 印も、大きさ n の x 印としてカウントしてしまう。
count :: UArray (Int, Int) Char -> Int -> Int
count mat n = length $ filter (isCrossMark mat n) centers
  where
    (h, w) = both succ . snd $ bounds mat
    -- 正方形の中心座標の一覧
    centers = squareCenters n (h, w)

-- | 累積和を元の数列に戻す
invCSum :: [Int] -> [Int]
invCSum = snd . mapAccumL step s0
  where
    s0 = 0 :: Int
    step lastX x = (x, x - lastX)

main :: IO ()
main = do
  [h, w] <- getInts
  mat <- getMat h w

  let nMax = min h w
  let counts = map (count mat) [1 .. nMax]
  let result = reverse . invCSum . reverse $ counts

  forM_ result print
