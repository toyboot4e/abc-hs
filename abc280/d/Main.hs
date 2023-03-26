#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers
-}

module Main (main) where

import Data.Char
import Data.List

{- ORMOLU_DISABLE -}

import qualified Data.ByteString.Char8 as BS

-- utility-ht: https://www.stackage.org/lts-16.11/package/utility-ht-0.0.15
import Data.Bool.HT  -- if', ..

{- ORMOLU_ENABLE -}

-- }}}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | Returns `[(prime, count)]`
primeFactors :: Int -> [(Int, Int)]
primeFactors n_ = map (\xs -> (head xs, length xs)) . group $ loop n_ input
  where
    input = 2 : 3 : [y | x <- [5, 11 ..], y <- [x, x + 2]]
    loop n pps@(p : ps)
      | n == 1 = []
      | n < p * p = [n]
      | r == 0 = p : loop q pps
      | otherwise = loop n ps
      where
        (q, r) = divMod n p

-- Get the biggest i s.t. n == 0 (mod p^i)
countDiv :: (Integral t, Num p) => t -> t -> p
countDiv n p =
  case n `quotRem` p of
    (0, _) -> 0
    (d, 0) -> 1 + countDiv d p
    _ -> 0

main :: IO ()
main = do
  [k] <- getLineIntList
  let pns = primeFactors k

  let mins = map f pns
      f (p, n) = fst $ until ((<= 0) . snd) g (p, n)
        where
          -- p -> 2p -> 3p -> ..
          g (acc, rest) =
            let c = countDiv acc p
                rest' = rest - c
                acc' = if' (rest' <= 0) acc (succ acc)
             in (acc', rest')

  -- let !_ = traceShow (k, mins) ()
  print $ maximum mins
