#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

{-# LANGUAGE BangPatterns #-}

import Data.Ix (inRange)
import Data.Maybe
import Data.Tuple.Extra (both)
import qualified Data.Vector.Unboxed as VU

-- | 素数の無限リスト。
-- | <https://zenn.dev/osushi0x/scraps/51ff0594a1e863#comment-1022553732563c>
primes :: [Int]
primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])
  where
    minus (x : xs) (y : ys) = case (compare x y) of
      LT -> x : minus xs (y : ys)
      EQ -> minus xs ys
      GT -> minus (x : xs) ys
    minus xs _ = xs

    union (x : xs) (y : ys) = case (compare x y) of
      LT -> x : union xs (y : ys)
      EQ -> x : union xs ys
      GT -> y : union (x : xs) ys
    union xs [] = xs
    union [] ys = ys

    unionAll :: Ord a => [[a]] -> [a]
    unionAll ((x : xs) : t) = x : union xs (unionAll $ pairs t)
      where
        pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t

-- | 境界の添字を返す 2 分探索。
-- |
-- | # 例
-- |
-- | `(<= 5)` という `isOk` 関数が与えられた場合、リスト `[0..9]` は以下のように見られる:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | この場合、 `(Just 5, Just 6)` というペアが返却される。
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (!low, !high) !isOk = both wrap (inner (low - 1, high + 1))
  where
    inner :: (Int, Int) -> (Int, Int)
    inner (!ok, !ng)
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

-- | `Int` 型の平方根 (切り落とし)
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO ()
main = do
  n <- readLn :: IO Int

  let pMax = isqrt (n `div` 12) + 1
  let ps = VU.fromList $ takeWhile (<= pMax) primes
  let nps = VU.length ps

  -- Int 型の取れる範囲は +- 2^63 ~ 9 * 10^18 程度。
  -- p <= sqrt 10^12 より p <= 10^6 であることから、オーバーフローに注意する必要がある。

  let result =
        [ count
          | ia <- [0 .. nps - 3],
            let a = ps VU.! ia,
            ib <- [ia + 1 .. nps - 2],
            let b = ps VU.! ib,
            let aab = a * a * b,
            aab <= n,
            let icMax = fromMaybe (-1) . fst $ bsearch (ib + 1, VU.length ps - 1) $ \ic ->
                  let c = ps VU.! ic
                   in aab * c <= n && aab * c * c <= n,
            let count = max 0 (icMax - ib)
        ]

  print $ sum result
