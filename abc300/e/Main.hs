#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Ix
import Data.List
import Data.Maybe
import Data.Proxy
import qualified Data.Ratio as Ratio
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

-- {{{ ダブリング

-- | Extends a function to be able to be applied multiple times in a constant time (N < 2^63).
newDoubling :: (VG.Vector v a, VG.Vector v Int) => a -> (a -> a) -> v a
newDoubling !x0 !squareF = VG.scanl' step x0 $ VG.enumFromN (0 :: Int) 62
  where
    step !acc !_ = squareF acc

-- | Runs a function n times using a folding function `f`.
applyDoubling :: (VG.Vector v a) => v a -> b -> (b -> a -> b) -> Int -> b
applyDoubling !doubling !x0 !f !n = foldl' step x0 [0 .. 62]
  where
    step !acc !nBit =
      if testBit n nBit
        then f acc (doubling VG.! nBit)
        else acc

-- }}}

-- {{{ 繰り返し二乗法で mod 上の逆数計算

-- | Cache of base^i for iterative square method
powerModCache :: Int -> Int -> (Int, VU.Vector Int)
powerModCache !base !modulo = (modulo, doubling)
  where
    -- doubling = VU.scanl' (\ !x _ -> x * x `rem` modulo) base $ vRange (1 :: Int) 62
    doubling = newDoubling base (\x -> x * x `rem` modulo)

-- | Calculates base^i (mod p) from a cache
powerByCache :: Int -> (Int, VU.Vector Int) -> Int
powerByCache !power (!modulo, !cache) = foldl' step 1 [0 .. 62]
  where
    step !acc !nBit =
      if testBit power nBit
        then acc * (cache VU.! nBit) `rem` modulo
        else acc

-- | One-shot calcaulation of $x / d mod p$, using Fermat's little theorem
-- |
-- | 1/d = d^{p-2} (mod p) <=> d^p = d (mod p)
-- |   where the modulus is a prime number and `x` is not a mulitple of `p`
invModF :: Int -> Int -> Int
invModF !d !modulus = invModFC modulus (powerModCache d modulus)

-- | 1/x = x^{p-2} mod p <=> x^p = x mod p
-- |   where the modulus is a prime number
-- |
-- | and x^{p-2} is calculated with cache
invModFC :: Int -> (Int, VU.Vector Int) -> Int
invModFC !primeModulus = powerByCache (primeModulus - 2)

-- }}}

-- {{{ ModInt

-- | Type level constant `Int` value.
-- | TODO: Replace with `GHC.TypeNats`
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` where modulus operation is performed automatically.
newtype ModInt p = ModInt {toInt :: Int}
  deriving (Eq)

instance Show (ModInt p) where
  show = show . toInt

instance TypeInt p => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $ (x1 + x2) `mod` typeInt (Proxy @p)
  (ModInt !x1) * (ModInt !x2) = ModInt $ (x1 * x2) `mod` typeInt (Proxy @p)
  negate (ModInt !v) = ModInt $ (-v) `mod` typeInt (Proxy @p)
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

instance TypeInt p => Fractional (ModInt p) where
  -- reciprocal of x (inverse of x)
  recip (ModInt !x) = ModInt $ invModF x (typeInt (Proxy @p))
  fromRational !r = ModInt n / ModInt d
    where
      n = fromInteger $ Ratio.numerator r
      d = fromInteger $ Ratio.denominator r

-- }}}

-- {{{ 素因数分解

primeFactors :: Int -> [(Int, Int)]
primeFactors !n_ = map (\ !xs -> (head xs, length xs)) . group $ inner n_ input
  where
    input = 2 : 3 : [y | x <- [5, 11 ..], y <- [x, x + 2]]
    inner n pps@(p : ps)
      | n == 1 = []
      | n < p * p = [n]
      | r == 0 = p : inner q pps
      | otherwise = inner n ps
      where
        (q, r) = divMod n p
    inner _ _ = error "unreachable"

-- }}}

{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i) => a i e -> (e -> e) -> i -> m ()
modifyArray !ary !f !i = do
  !v <- f <$> readArray ary i
  writeArray ary i v

data MyModulus = MyModulus

instance TypeInt MyModulus where
  typeInt _ = 998244353

type MyModInt = ModInt MyModulus

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` typeInt (Proxy @MyModulus))

solve :: [(Int, Int)] -> MyModInt
solve !pns = result ! (j2, j3, j5)
  where
    !j2 = maybe 0 snd $ find ((== 2) . fst) pns
    !j3 = maybe 0 snd $ find ((== 3) . fst) pns
    !j5 = maybe 0 snd $ find ((== 5) . fst) pns
    !iMax = j2 `max` j3 `max` j5

    -- `runSTArray` で型推論させると楽
    !result = runSTArray $ do
      !arr <- newArray ((0, 0, 0), (iMax + 2, iMax + 2, iMax + 2)) (modInt 0)
      writeArray arr (0, 0, 0) (modInt 1)

      let div5 = modInt 1 / modInt 5
      forM_ (range ((0, 0, 0), (iMax, iMax, iMax))) $ \(!i2, !i3, !i5) -> do
        !p0 <- readArray arr (i2, i3, i5)
        let !p = p0 * div5

        modifyArray arr (+ p) (i2 + 1, i3, i5)
        modifyArray arr (+ p) (i2, i3 + 1, i5)
        modifyArray arr (+ p) (i2 + 2, i3, i5)
        modifyArray arr (+ p) (i2, i3, i5 + 1)
        modifyArray arr (+ p) (i2 + 1, i3 + 1, i5)

      return arr

main :: IO ()
main = do
  n <- readLn :: IO Int
  let !pns = primeFactors n

  print $
    if any ((`notElem` [2, 3, 5]) . fst) pns
      then modInt 0
      else solve pns
