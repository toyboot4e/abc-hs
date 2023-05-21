#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, DefaultSignatures, LambdaCase, MultiWayIf #-}
{-# LANGUAGE NumDecimals, NumericUnderscores, PatternGuards, TupleSections #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints, ScopedTypeVariables, StrictData, TypeApplications #-}
{-# LANGUAGE TypeFamilies, RankNTypes #-}

{-# LANGUAGE CPP, TemplateHaskell #-}
{- ORMOLU_ENABLE -}

{- TODO: on 2023 langauge update,
  - ditch `vector-th-unbox` and `TemplateHaskell`
  - remove `vLength`
  - refactor `primes` with new Prelude
-}

-- {{{ Imports

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.STRef
import Data.Word
import Debug.Trace
import GHC.Exts
import GHC.Float (int2Float)
import System.Exit (exitSuccess)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- base
import qualified Data.Ratio as Ratio

-- array
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe

import qualified Data.Array as A

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- extra: https://www.stackage.org/lts-16.11/package/extra-1.7.6
import Control.Monad.Extra hiding (loop) -- foldM, ..
import Data.IORef.Extra                  -- writeIORef'
import Data.List.Extra hiding (merge)    -- nubSort, ..
import Data.Tuple.Extra hiding (first, second)
import Numeric.Extra       -- showDP, intToFloat, ..

-- utility-ht: https://www.stackage.org/lts-16.11/package/utility-ht-0.0.15
import Data.Bool.HT  -- if', ..
import qualified Data.Ix.Enum as HT
import qualified Data.List.HT as HT -- `groupBy`, but with adjacent elements

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Bundle.Size    as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS

-- vector-algorithms: https://www.stackage.org/haddock/lts-16.31/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS

-- vector-th-unbox: https://www.stackage.org/lts-16.11/package/vector-th-unbox-0.2.1.7
import Data.Vector.Unboxed.Deriving (derivingUnbox)

-- containers: https://www.stackage.org/lts-16.11/package/containers-0.6.2.1
import qualified Data.Graph as G
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Sequence as Seq

-- heaps: https://www.stackage.org/haddock/lts-16.31/heaps-0.3.6.1/Data-Heap.html
import qualified Data.Heap as H

-- hashable: https://www.stackage.org/lts-16.11/package/hashable-1.3.0.0
import Data.Hashable

-- unordered-containers: https://www.stackage.org/haddock/lts-16.31/unordered-containers-0.2.10.0
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

{- ORMOLU_ENABLE -}

-- }}}

-- {{{ Debug-only utilities

-- When run as a stack script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()

dbgAssert :: Bool -> a -> a
dbgAssert False !x = error "assertion failed!"
dbgAssert True !x = x

#else
dbg :: Show a => a -> ()
dbg _ = ()

dbgAssert :: Bool -> a -> a
dbgAssert = flip const

#endif

-- }}}

-- {{{ Prelude utilities

-- | From more recent GHC
clamp :: (Ord a) => (a, a) -> a -> a
clamp (!low, !high) !a = min high (max a low)

flipOrder :: Ordering -> Ordering
flipOrder = \case
  GT -> LT
  LT -> GT
  EQ -> EQ

square :: Num a => a -> a
square !x = x * x

-- }}}

-- {{{ More extras

-- | Two-variable function compositon.
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(.:) = (.) . (.)

-- | Three-variable function compositon.
(.:.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> (a1 -> a2 -> a3 -> c)
(.:.) = (.) . (.) . (.)

foldForM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldForM !s0 !xs !m = foldM m s0 xs

foldForMVG :: (PrimMonad m, VG.Vector v a) => b -> v a -> (b -> a -> m b) -> m b
foldForMVG !s0 !xs !m = VG.foldM m s0 xs

-- }}}

-- {{{ Libary complements

{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i) => a i e -> (e -> e) -> i -> m ()
modifyArray !ary !f !i = do
  !v <- f <$> readArray ary i
  writeArray ary i v

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE rangeVG #-}
rangeVG :: (VG.Vector v Int) => Int -> Int -> v Int
rangeVG !i !j = VG.enumFromN i (succ j - i)

-- | `rangeVG` in reverse.
{-# INLINE rangeVGR #-}
rangeVGR :: (VG.Vector v Int) => Int -> Int -> v Int
rangeVGR !i !j = VG.enumFromStepN i (-1) (succ j - i)

-- | @cojna (`stream`)
{-# INLINE [1] rangeMS #-}
rangeMS :: (Monad m) => Int -> Int -> MS.Stream m Int
rangeMS !l !r = MS.Stream step l
  where
    {-# INLINE [0] step #-}
    step x
      | x <= r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done

-- | @cojna (`streamR`)
{-# INLINE [1] rangeMSR #-}
rangeMSR :: (Monad m) => Int -> Int -> MS.Stream m Int
rangeMSR !l !r = MS.Stream step r
  where
    {-# INLINE [0] step #-}
    step x
      | x >= l = return $ MS.Yield x (x - 1)
      | otherwise = return MS.Done

-- | `forM` over monadic stream in the vector package.
-- | NOTE: This is for side effects only. I don't know how to use `MS.mapM` yet.
{-# INLINE forMS_ #-}
forMS_ :: (Monad m) => MS.Stream m Int -> (Int -> m ()) -> m ()
forMS_ = flip MS.mapM_

-- }}}

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- compress deduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

-- | Runs the given function `n` times.
times :: Int -> (a -> a) -> a -> a
times !n !f !s0 = snd $ until ((== n) . fst) (bimap succ f) (0 :: Int, s0)

-- | Returns combinations of the list taking n values.
-- | For example, binary combinations are got by `combination 2 [0..8]`.
-- | REMARK: This is slow. Prefer list comprehension like `x <- [1 .. n], y <- [x + 1 .. n]m ..]`.
combinations :: Int -> [a] -> [[a]]
combinations !len !elements = comb len (length elements) elements
  where
    comb 0 _ _ = [[]]
    comb !r !n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
    comb _ _ _ = error "unreachable"

-- | Returns inclusive ranges that satisfy the given `check`.
-- TODO: cheaper implementation
twoPointers :: Int -> ((Int, Int) -> Bool) -> [(Int, Int)]
twoPointers !n !check = inner (0, 0)
  where
    inner (!l, !r) | l >= n = []
    inner (!l, !r)
      | check (l, r) =
          let (!l', !r') = until (not . peekCheck) (second succ) (l, r)
           in (l', r') : inner (succ l', max l' r')
      | otherwise = inner (succ l, max (succ l) r)
    peekCheck (!l, !r) | r == pred n = False
    peekCheck (!l, !r) = check (l, succ r)

-- }}}

-- {{{ Tuples

tuple2 :: [a] -> (a, a)
tuple2 [!a, !b] = (a, b)
tuple2 _ = error "not a two-item list"

tuple3 :: [a] -> (a, a, a)
tuple3 [!a, !b, !c] = (a, b, c)
tuple3 _ = error "not a three-item list"

tuple4 :: [a] -> (a, a, a, a)
tuple4 [!a, !b, !c, !d] = (a, b, c, d)
tuple4 _ = error "not a four-item list"

ints2 :: IO (Int, Int)
ints2 = tuple2 <$> ints

ints3 :: IO (Int, Int, Int)
ints3 = tuple3 <$> ints

ints4 :: IO (Int, Int, Int, Int)
ints4 = tuple4 <$> ints

yn :: Bool -> String
yn b = if b then "Yes" else "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

-- | `concat` two-item tuples
concat2 :: [(a, a)] -> [a]
concat2 [] = []
concat2 ((!x, !y) : xys) = x : y : concat2 xys

concatMap2 :: (a -> (b, b)) -> [a] -> [b]
concatMap2 !f = concat2 . map f

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (!y, !x) = bimap (y +) (x +)

sub2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub2 (!y, !x) = bimap (y -) (x -)

mul2 :: Int -> (Int, Int) -> (Int, Int)
mul2 !m = both (m *)

-- 180.0 degree = \p radian
toRadian :: Double -> Double
toRadian degree = degree / 180.0 * pi

toDegree :: Double -> Double
toDegree rad = rad / pi * 180.0

-- }}}

-- {{{ Input

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsVG :: VG.Vector v Int => IO (v Int)
intsVG = VG.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsV :: IO (V.Vector Int)
intsV = intsVG

intsVU :: IO (VU.Vector Int)
intsVU = intsVG

-- | Creates a graph from 1-based vertices
getGraph :: Int -> Int -> IO (Array Int [Int])
getGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ second swap . dupe . tuple2

-- | Creates a weightend graph from 1-based vertices
getWGraph :: Int -> Int -> IO (Array Int [H.Entry Int Int])
getWGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ \[!a, !b, !cost] -> ((a, H.Entry cost b), (b, H.Entry cost a))

-- | Creates a weightend graph from 1-based vertices
getWGraph0 :: Int -> Int -> IO (Array Int [H.Entry Int Int])
getWGraph0 !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (0, pred nVerts)
    toInput = concatMap2 $ \[!a, !b, !cost] -> ((pred a, H.Entry cost (pred b)), (pred b, H.Entry cost (pred a)))

-- }}}

-- {{{ Output

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

putLnBSB :: BSB.Builder -> IO ()
putLnBSB = BSB.hPutBuilder stdout . (<> endlBSB)

-- ord8 :: Char -> Word8
-- ord8 = fromIntegral . fromEnum
--
-- chr8 :: Word8 -> Char
-- chr8 = toEnum . fromIntegral

-- | Show as a bytestring builder
class ShowBSB a where
  showBSB :: a -> BSB.Builder
  default showBSB :: (Show a) => a -> BSB.Builder
  showBSB = BSB.string8 . show

instance ShowBSB Int where
  showBSB = BSB.intDec

instance ShowBSB Integer where
  showBSB = BSB.integerDec

instance ShowBSB Float where
  showBSB = BSB.floatDec

instance ShowBSB Double where
  showBSB = BSB.doubleDec

showLnBSB :: ShowBSB a => a -> BSB.Builder
showLnBSB = (<> endlBSB) . showBSB

printBSB :: ShowBSB a => a -> IO ()
printBSB = putBSB . showBSB

-- | Often used as `concatBSB showBSB xs` or `concatB showLnBSB xs`.
concatBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
concatBSB f = VG.foldr ((<>) . f) mempty

-- }}}

-- {{{ Trace

-- TODO: merge them with `dbg` series.

traceMat2D :: (IArray a e, Ix i, Show e) => a (i, i) e -> ()
traceMat2D !mat = traceSubMat2D mat (bounds mat)

traceSubMat2D :: (IArray a e, Ix i, Show e) => a (i, i) e -> ((i, i), (i, i)) -> ()
traceSubMat2D !mat ((!y0, !x0), (!yEnd, !xEnd)) =
  let !_ = foldl' step () (range ys)
   in ()
  where
    !xs = (y0, yEnd)
    !ys = (x0, xEnd)
    step !_ !y = traceShow (map (\ !x -> mat ! (y, x)) (range xs)) ()

-- }}}

-- {{{ Math

mulMat :: (Num e, IArray UArray e) => UArray (Int, Int) e -> UArray (Int, Int) e -> UArray (Int, Int) e
mulMat a b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))]
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((i0, j0), (ix, jx)) = bounds a
    ((j'0, k0), (j'x, kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

mulMatMod :: Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int
mulMatMod m a b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) `mod` m | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))] `mod` m
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((i0, j0), (ix, jx)) = bounds a
    ((j'0, k0), (j'x, kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

-- }}}

-- {{{ Digits

-- Taken from <https://hackage.haskell.org/package/digits-0.3.1/docs/Data-Digits.html>

-- digitToInt :: Char -> Int

-- | Returns the digits of a positive integer as a Maybe list, in reverse order or Nothing if a zero
-- | or negative base is given. This is slightly more efficient than in forward order.
mDigitsRev :: Integral n => n -> n -> Maybe [n]
mDigitsRev !base !i = if base < 1 then Nothing else Just $ dr base i
  where
    dr _ 0 = []
    dr !b !x = case base of
      1 -> genericTake x $ repeat 1
      _ ->
        let (!rest, !lastDigit) = quotRem x b
         in lastDigit : dr b rest

-- | Returns the digits of a positive integer as a Maybe list.
--   or Nothing if a zero or negative base is given
mDigits :: Integral n => n -> n -> Maybe [n]
mDigits !base !i = reverse <$!> mDigitsRev base i

-- | Returns the digits of a positive integer as a list, in reverse order.
--   Throws an error if given a zero or negative base.
digitsRev :: Integral n => n -> n -> [n]
digitsRev !base = fromJust . mDigitsRev base

-- | Returns the digits of a positive integer as a list.
-- | REMARK: It's modified to return `[0]` when given zero.
digits :: (Eq n, Integral n) => n -> n -> [n]
digits _ 0 = [0]
digits !base !x = reverse $ digitsRev base x

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n => n -> [n] -> n
unDigits !base = foldl' (\ !a !b -> a * base + b) 0

-- | <https://stackoverflow.com/questions/10028213/converting-number-base>
-- | REMARK: It returns `[]` when given `[0]`. Be sure to convert `[]` to `[0]` if necessary.
convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase !from !to = digits to . unDigits from

-- }}}

-- {{{ Bits

-- TODO: super efficient bit operations

-- | Log base of two or bit floor.
-- | <https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros>
log2 :: (FiniteBits b) => b -> Int
log2 !x = finiteBitSize x - 1 - countLeadingZeros x

-- | Ceiling of log base 2 of an `Int`.
-- |
-- | # Example
-- |
-- | ```hs
-- | > log2 3
-- | 1
-- | > log2CeilInt 3
-- | 2
-- | ```
log2CeilInt :: Int -> Int
log2CeilInt !x = msb + ceiling_
  where
    !msb = log2 x
    !ceiling_ = if clearBit x msb > 0 then 1 else 0

-- | Calculates the smallest integral power of two that is not smaller than `x`.
-- |
-- | # Example
-- |
-- | ```hs
-- | > bitCeil 3
-- | 4
-- | ```
bitCeil :: Int -> Int
bitCeil = bit . log2CeilInt

-- }}}

-- {{{ Integer

-- | CAUTION: Be aware of the accuracy. Prefer binary search when possible
isqrt :: Int -> Int
isqrt = round @Double . sqrt . fromIntegral

-- | Calculates `x * y` but wrapping the result to the maximum boundary.
-- | Works for x >= 0 only.
wrappingMul :: Int -> Int -> Int
wrappingMul !x !y =
  if (64 - countLeadingZeros x) + (64 - countLeadingZeros y) > 63
    then maxBound @Int
    else x * y

-- }}}

-- {{{ Prime factors

-- -- @gotoki_no_joe
-- primes :: [Int]
-- primes = 2 : 3 : sieve q0 [5, 7 ..]
--   where
--     q0 = H.insert (H.Entry 9 6) H.empty
--     sieve queue xxs@(x : xs) =
--       case compare np x of
--         LT -> sieve queue1 xxs
--         EQ -> sieve queue1 xs
--         GT -> x : sieve queue2 xs
--       where
--         H.Entry np p2 = H.minimum queue
--         queue1 = H.insert (H.Entry (np + p2) p2) $ H.deleteMin queue
--         queue2 = H.insert (H.Entry (x * x) (x * 2)) queue
--     sieve _ _ = error "unreachale"

-- | @0xYusuke
-- | https://zenn.dev/link/comments/1022553732563c
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

-- | Returns `[(prime, count)]`
-- TODO: reuse `primes`
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

-- {{{ WIP: monoid-based binary lifting system

-- TODO: `invModF` -> `invModD` (doubling)

-- | OperatorMonoid mapp mact
-- |
-- | a `mact` m1 `mact` m2 = a `mact` (m1 `mapp` m2)
-- |
-- | Such a monoid is called an operator monoid.
-- | Doulbing, DP with rerooting and lazy segment tree make use of them. I guess.
data OperatorMonoid m a = OperatorMonoid m (m -> m -> m) (a -> m -> a)

data BinaryLifting m a v = BinaryLifting (OperatorMonoid m a) (v m)

-- TODO: Test the opereator monoid doulbing template and remove raw functions:
binLift :: (VG.Vector v m, VG.Vector v Int) => OperatorMonoid m a -> BinaryLifting m a v
binLift opMonoid@(OperatorMonoid !op0 !mapp !_) = BinaryLifting opMonoid ops
  where
    step !op !_ = op `mapp` op
    !ops = VG.scanl' step op0 $ VG.enumFromN (0 :: Int) 62

binLiftReplacement :: VU.Vector Int -> BinaryLifting (VU.Vector Int) Int V.Vector
binLiftReplacement !op0 =
  binLift $ OperatorMonoid op0 (\op1 op2 -> VU.map (op2 VU.!) op1) (flip (VU.!))

-- | Binarily lifted operator monoid action application.
mactB :: (VG.Vector v m) => (BinaryLifting m a v) -> a -> Int -> a
mactB (BinaryLifting (OperatorMonoid !_ !_ !mact) !ops) !acc0 !nAct = VU.foldl' step acc0 (rangeVG 0 62)
  where
    step !acc !nBit
      | testBit nAct nBit = acc `mact` (ops VG.! nBit)
      | otherwise = acc

-- }}}

-- {{{ Doubling

-- | Extends an operator to be able to be applied multiple times in a constant time (N < 2^63).
newDoubling :: (VG.Vector v a, VG.Vector v Int) => a -> (a -> a) -> v a
newDoubling !oper0 !squareCompositeF = VG.scanl' step oper0 $ VG.enumFromN (0 :: Int) 62
  where
    step !oper !_ = squareCompositeF oper

newDoublingV :: a -> (a -> a) -> V.Vector a
newDoublingV = newDoubling

-- | Applies an operator `n` times using an applying function `applyF`.
applyDoubling :: (VG.Vector v a) => v a -> b -> (b -> a -> b) -> Int -> b
applyDoubling !opers !x0 !applyF !n = foldl' step x0 [0 .. 62]
  where
    !_ = dbgAssert $ VG.length opers == 63
    step !acc !nBit =
      if testBit n nBit
        then applyF acc (opers VG.! nBit)
        else acc

-- }}}

-- {{{ Modulo arithmetic

-- TODO: refactor
-- TODO: consider taking `modulus` as the first argument

addMod, subMod, mulMod :: Int -> Int -> Int -> Int
addMod !x !a !modulus = (x + a) `mod` modulus
subMod !x !s !modulus = (x - s) `mod` modulus
mulMod !b !p !modulus = (b * p) `mod` modulus

-- | n! `mod` m
factMod :: Int -> Int -> Int
factMod 0 _ = 1
factMod 1 _ = 1
factMod !n !m = n * factMod (n - 1) m `rem` m

-- F: Fermet, FC: Fermet by cache

-- | One-shot calculation of $base ^ power `mod` modulo$ in a constant time
powerModConstant :: Int -> Int -> Int -> Int
powerModConstant !base !power !modulo = powerByCache power (powerModCache base modulo)

-- | One-shot calcaulation of $x / d mod p$, using Fermat's little theorem
-- |
-- | 1/d = d^{p-2} (mod p) <=> d^p = d (mod p)
-- |   where the modulus is a prime number and `x` is not a mulitple of `p`
invModF :: Int -> Int -> Int
invModF !d !modulus = invModFC modulus (powerModCache d modulus)

-- | x / d mod p, using Fermat's little theorem
-- |
-- | 1/d = d^{p-2} (mod p) <=> d^p = d (mod p)
-- |   where the modulus is a prime number and `x` is not a mulitple of `p`
divModF :: Int -> Int -> Int -> Int
divModF !x !d !modulus = divModFC x (powerModCache d modulus) `rem` modulus

-- | Cache of base^i for iterative square method
powerModCache :: Int -> Int -> (Int, VU.Vector Int)
powerModCache !base !modulo = (modulo, doubling)
  where
    -- doubling = VU.scanl' (\ !x _ -> x * x `rem` modulo) base $ rangeVG (1 :: Int) 62
    doubling = newDoubling base (\x -> x * x `rem` modulo)

-- | Calculates base^i (mod p) from a cache
powerByCache :: Int -> (Int, VU.Vector Int) -> Int
-- TODO: test if it works as expeted
-- powerByCache !power (!modulo, !cache) = applyDoubling cache 1 (\acc x -> acc * x `rem` modulo) power
powerByCache !power (!modulo, !cache) = foldl' step 1 [0 .. 62]
  where
    step !acc !nBit =
      if testBit power nBit
        then acc * (cache VU.! nBit) `rem` modulo
        else acc

-- | 1/x = x^{p-2} mod p <=> x^p = x mod p
-- |   where the modulus is a prime number
-- |
-- | and x^{p-2} is calculated with cache
invModFC :: Int -> (Int, VU.Vector Int) -> Int
invModFC !primeModulus = powerByCache (primeModulus - 2)

divModFC :: Int -> (Int, VU.Vector Int) -> Int
divModFC !x context@(!modulus, !_) = x * invModFC modulus context `rem` modulus

-- | Cache of `n! mod m` up to `n`.
factMods :: Int -> Int -> VU.Vector Int
factMods !n !modulus =
  VU.scanl' (\ !x !y -> x * y `rem` modulus) (1 :: Int) $ VU.fromList [(1 :: Int) .. n]

-- | nCr `mod` m (binominal cofficient)
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulus = foldl' (\ !x !y -> divModF x y modulus) (facts VU.! n) [facts VU.! r, facts VU.! (n - r)]
  where
    facts = factMods n modulus

-- }}}

-- {{{ ModInt

-- | Type level constant `Int` value.
-- | TODO: Replace with `GHC.TypeNats`
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` where modulus operation is performed automatically.
newtype ModInt p = ModInt {toInt :: Int}
  deriving (Eq)

derivingUnbox
  "ModInt"
  [t|forall p. ModInt p -> Int|]
  [|\(ModInt !x) -> x|]
  [|\ !x -> ModInt x|]

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

-- {{{ Rolling hash

-- | Rolling hash of a string.
-- |
-- | # Example
-- |
-- | Slice (2, 4) of "abcdef" is given as this:
-- | ```
-- |            s :=     a       b       c       d       e
-- |            s4 = b^4 a + b^3 b + b^2 c + b^1 d + b^0 e
-- |            s2 = b^1 a + b^0 b
-- | s4 - s2 * b^3 =                 b^2 c + b^1 d + b^0 e
-- | ```
data RollingHash b p = RollingHash
  { sourceLength :: !Int,
    -- | \$\{B^i mod p\}_{i \elem [0, n)}$
    dimensions :: !(VU.Vector Int),
    hashSum :: !(VU.Vector Int)
  }
  deriving (Show, Eq)

-- | B-adic number for the rolling hash algorithm.
data HashInt = HashInt

instance TypeInt HashInt where
  typeInt _ = 100

newRHash :: forall p. TypeInt p => String -> RollingHash HashInt p
newRHash !source = RollingHash n bn hashSum
  where
    !p = typeInt (Proxy @p)
    !b = typeInt (Proxy @HashInt)
    !n = length source
    !bn = VU.create $ do
      !vec <- VUM.replicate n (1 :: Int)
      forMS_ (rangeMS 1 (pred n)) $ \i -> do
        !lastB <- VUM.unsafeRead vec (pred i)
        VUM.unsafeWrite vec i (b * lastB `mod` p)
      return vec
    !hashSum = evalState (VU.mapM (\ !ch -> state $ \ !acc -> f ch acc) $ VU.fromList source) (0 :: Int)
      where
        f :: Char -> Int -> (Int, Int)
        f !ch !lastX = dupe $ (lastX * b + ord ch) `mod` p

lengthRHash :: RollingHash b p -> Int
lengthRHash (RollingHash !len !_ !_) = len

-- | HashSlice value length. See also the example of `RollingHash`.
data HashSlice p = HashSlice
  { hashValue :: {-# UNPACK #-} !Int,
    -- hashOffset :: {-# UNPACK #-} !Int,
    hashLength :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)

-- | Slices a rolling hash string
sliceRHash :: forall b p. (TypeInt b, TypeInt p) => RollingHash b p -> Int -> Int -> HashSlice p
sliceRHash (RollingHash !_ !bn !s) !i0 !i1
  -- TODO: add debug assertion
  | i0 > i1 = HashSlice 0 0
  | otherwise =
      let !len = i1 - i0 + 1
          !s1 = s VU.! i1
          !s0 = fromMaybe 0 $ s VU.!? pred i0
          !value = (s1 - (bn VU.! len) * s0) `mod` p
       in HashSlice value len
  where
    !p = typeInt (Proxy @p)

consHashSlice :: forall b p. (TypeInt b, TypeInt p) => RollingHash b p -> HashSlice p -> HashSlice p -> HashSlice p
consHashSlice (RollingHash !_ !bn !_) (HashSlice !v0 !l0) (HashSlice !v1 !l1) = HashSlice value len
  where
    !p = typeInt (Proxy @p)
    !value = ((bn VU.! l1) * v0 + v1) `mod` p
    !len = l0 + l1

-- }}}

-- {{{ Multiset

-- | Multiset: (nKeys, (key -> count))
type MultiSet = (Int, IM.IntMap Int)

emptyMS :: MultiSet
emptyMS = (0, IM.empty)

singletonMS :: Int -> MultiSet
singletonMS !x = (1, IM.singleton x 1)

fromListMS :: [Int] -> MultiSet
fromListMS = foldl' (flip incrementMS) emptyMS

incrementMS :: Int -> MultiSet -> MultiSet
incrementMS !k (!n, !im) =
  if IM.member k im
    then (n, IM.insertWith (+) k 1 im)
    else (n + 1, IM.insert k 1 im)

decrementMS :: Int -> MultiSet -> MultiSet
decrementMS !k (!n, !im) =
  case IM.lookup k im of
    Just 1 -> (n - 1, IM.delete k im)
    Just _ -> (n, IM.insertWith (+) k (-1) im)
    Nothing -> (n, im)

memberMS :: Int -> MultiSet -> Bool
memberMS !k (!_, !im) = IM.member k im

notMemberMS :: Int -> MultiSet -> Bool
notMemberMS !k (!_, !im) = IM.notMember k im

deleteFindMinMS :: MultiSet -> (Int, MultiSet)
deleteFindMinMS ms@(!n, !im) =
  let !key = fst $ IM.findMin im
   in (key, decrementMS key ms)

innerMS :: MultiSet -> IM.IntMap Int
innerMS (!_, !im) = im

-- }}}

-- {{{ Queue (just for remembering how to use `Seq`)

enqueue :: a -> Seq.Seq a -> Seq.Seq a
enqueue !x !s = x Seq.<| s

dequeue :: Seq.Seq a -> (a, Seq.Seq a)
dequeue (x Seq.:<| s) = (x, s)
dequeue _ = error "unable to dequeue from empty sequence"

dequeueMaybe :: Seq.Seq a -> Maybe (a, Seq.Seq a)
dequeueMaybe (x Seq.:<| s) = Just (x, s)
dequeueMaybe _ = Nothing

-- }}}

-- {{{ ismo 2D

ismo2D :: ((Int, Int), (Int, Int)) -> UArray (Int, Int) Int -> UArray (Int, Int) Int
ismo2D !bounds_ !seeds = runSTUArray $ do
  arr <- newArray bounds_ (0 :: Int)

  -- row scan
  forM_ (range bounds_) $ \(!y, !x) -> do
    !v <- if x == 0 then return 0 else readArray arr (y, x - 1)
    let !diff = seeds ! (y, x)
    writeArray arr (y, x) (v + diff)

  -- column scan
  forM_ (range bounds_) $ \(!x, !y) -> do
    !v <- if y == 0 then return 0 else readArray arr (y - 1, x)
    !diff <- readArray arr (y, x)
    writeArray arr (y, x) (v + diff)

  return arr

-- }}}

-- {{{ Binary search

-- TODO: Use typeclass for getting middle and detecting end

-- | Pure variant of [`bsearchM`].
{-# INLINE bsearch #-}
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch !rng = runIdentity . bsearchM rng . (return .)

-- | Also known as lower bound.
{-# INLINE bsearchL #-}
bsearchL :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bsearchL = fst .: bsearch

-- | Also known as upper bound.
{-# INLINE bsearchR #-}
bsearchR :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bsearchR = snd .: bsearch

-- | Monadic binary search for sorted items in an inclusive range (from left to right only).
-- |
-- | It returns an `(ok, ng)` index pair at the boundary.
-- |
-- | # Example
-- |
-- | With an OK predicate `(<= 5)`, list `[0..9]` can be seen as:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | In this case `bsearch` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearch (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
{-# INLINE bsearchM #-}
bsearchM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bsearchM (!low, !high) !isOk = both wrap <$> inner (low - 1, high + 1)
  where
    inner :: (Int, Int) -> m (Int, Int)
    inner (!ok, !ng)
      | abs (ok - ng) == 1 = return (ok, ng)
      | otherwise =
          isOk m >>= \case
            True -> inner (m, ng)
            False -> inner (ok, m)
      where
        m = (ok + ng) `div` 2

    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

{-# INLINE bsearchML #-}
bsearchML :: forall m. (Applicative m, Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bsearchML = fmap fst .: bsearchM

{-# INLINE bsearchMR #-}
bsearchMR :: forall m. (Applicative m, Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bsearchMR = fmap snd .: bsearchM

{-# INLINE bsearchF32 #-}
bsearchF32 :: (Float, Float) -> Float -> (Float -> Bool) -> (Maybe Float, Maybe Float)
bsearchF32 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
  where
    inner :: (Float, Float) -> (Float, Float)
    inner (!ok, !ng)
      | abs (ok - ng) <= diff = (ok, ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        m = (ok + ng) / 2
    wrap :: Float -> Maybe Float
    wrap !x
      | x == (low - diff) || x == (high + diff) = Nothing
      | otherwise = Just x

{-# INLINE bsearchF32L #-}
bsearchF32L :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bsearchF32L = fst .:. bsearchF32

{-# INLINE bsearchF32R #-}
bsearchF32R :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bsearchF32R = fst .:. bsearchF32

{-# INLINE bsearchF64 #-}
bsearchF64 :: (Double, Double) -> Double -> (Double -> Bool) -> (Maybe Double, Maybe Double)
bsearchF64 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
  where
    inner :: (Double, Double) -> (Double, Double)
    inner (!ok, !ng)
      | abs (ok - ng) < diff = (ok, ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        m = (ok + ng) / 2
    wrap :: Double -> Maybe Double
    wrap !x
      | x == (low - diff) || x == (high + diff) = Nothing
      | otherwise = Just x

{-# INLINE bsearchF64L #-}
bsearchF64L :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bsearchF64L !a !b !c = fst $ bsearchF64 a b c

{-# INLINE bsearchF64R #-}
bsearchF64R :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bsearchF64R !a !b !c = fst $ bsearchF64 a b c

-- 1D index compression: xs -> (nubSorted, indices)
compressIndex :: [Int] -> (VU.Vector Int, [Int])
compressIndex xs = (indices, map (fromJust . fst . f) xs)
  where
    !indices = VU.fromList $ nubSort xs
    f !x = bsearch (0, pred $ vLength indices) $ \i -> indices VU.! i <= x

-- 1D index compression: xs -> (indices, xs')
compressVU :: VU.Vector Int -> (VU.Vector Int, VU.Vector Int)
compressVU xs = (indices, VU.map (fromJust . fst . f) xs)
  where
    !indices = VU.fromList $ nubSort $ VU.toList xs
    f !x = bsearch (0, pred $ vLength indices) $ \i -> indices VU.! i <= x

-- }}}

-- {{{ Dense, mutable union-Find tree

-- | Dense, mutable union-find tree (originally by `@pel`)
newtype MUnionFind s = MUnionFind (VUM.MVector s MUFNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | `MUFChild parent | MUFRoot size`.
data MUFNode = MUFChild {-# UNPACK #-} !Int | MUFRoot {-# UNPACK #-} !Int

derivingUnbox
  "MUFNode"
  [t|MUFNode -> (Bool, Int)|]
  [|\case (MUFChild !x) -> (True, x); (MUFRoot !x) -> (False, x)|]
  [|\case (True, !x) -> MUFChild x; (False, !x) -> MUFRoot x|]

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF !n = MUnionFind <$> VUM.replicate n (MUFRoot 1)

-- | Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
rootMUF uf@(MUnionFind !vec) i = do
  !node <- VUM.unsafeRead vec i
  case node of
    MUFRoot _ -> return i
    MUFChild p -> do
      !r <- rootMUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VUM.unsafeWrite vec i (MUFChild r)
      return r

-- | Checks if the two nodes are under the same root.
{-# INLINE sameMUF #-}
sameMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
sameMUF !uf !x !y = liftM2 (==) (rootMUF uf x) (rootMUF uf y)

-- | Just an internal helper.
_unwrapMUFRoot :: MUFNode -> Int
_unwrapMUFRoot (MUFRoot !s) = s
_unwrapMUFRoot (MUFChild !_) = undefined

-- | Unites two nodes.
{-# INLINE uniteMUF #-}
uniteMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m ()
uniteMUF uf@(MUnionFind !vec) !x !y = do
  !px <- rootMUF uf x
  !py <- rootMUF uf y
  when (px /= py) $! do
    !sx <- _unwrapMUFRoot <$!> VUM.unsafeRead vec px
    !sy <- _unwrapMUFRoot <$!> VUM.unsafeRead vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (!par, !chld) = if sx < sy then (px, py) else (py, px)
    VUM.unsafeWrite vec chld (MUFChild par)
    VUM.unsafeWrite vec par (MUFRoot (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind !vec) !x = do
  !px <- rootMUF uf x
  _unwrapMUFRoot <$!> VUM.unsafeRead vec px

-- }}}

-- {{{ Sparse, immutable union-find tree

-- @gotoki_no_joe
type SparseUnionFind = IM.IntMap Int

newSUF :: SparseUnionFind
newSUF = IM.empty

rootSUF :: SparseUnionFind -> Int -> (Int, Int)
rootSUF !uf !i
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, -j)
  | otherwise = rootSUF uf j
  where
    j = uf IM.! i

findSUF :: SparseUnionFind -> Int -> Int -> Bool
findSUF !uf !i !j = fst (rootSUF uf i) == fst (rootSUF uf j)

uniteSUF :: SparseUnionFind -> Int -> Int -> SparseUnionFind
uniteSUF !uf !i !j
  | a == b = uf
  | r >= s = IM.insert a (negate $ r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $ r + s) $ IM.insert a b uf
  where
    (!a, !r) = rootSUF uf i
    (!b, !s) = rootSUF uf j

-- }}}

-- {{{ Segment tree

-- TODO: refactor

-- | A mutable segment tree backed by a complete binary tree.
-- |
-- | # Overview
-- |
-- | A segment tree is a cache of a folding function.
-- | Each node corresponds to a folding range and the node contains the folding result.
-- |
-- | A segment tree has a constant size and never be resized.
-- |
-- | # Operations
-- |
-- | Modification takes $O(log N)$, so creation takes $N(log N)$.
-- | Lookup takes $O(log N)$.
-- |
-- | # (Internal) Indices
-- |
-- | The complete binary tree has `2 ^ depth - 1` elements.
-- |
-- | - Child elements of a parent node `i` has index `2 * i + 1` and `2 * i + 2`.
-- | - The leaf indices start with `length / 2 - 1`.
-- |
-- | Example:
-- |
-- | ```
-- |            0
-- |      1           2
-- |   3     4     5     6
-- | 07 08 09 10 11 12 13 14
-- | ```
data MSegmentTree v s a = MSegmentTree (a -> a -> a) (v s a)

-- TODO: Can I UNPACK? the funciton?
-- TODO: Possibly a show instance?

-- | Creates a new segment tree for `n` leaves.
-- | REMARK: Always give a zero value. It fills all the nodes including parent nodes, and the parent
-- | nodes are not updated.
{-# INLINE newSTreeVG #-}
newSTreeVG :: (VGM.MVector v a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree v (PrimState m) a)
newSTreeVG !f !n !value = MSegmentTree f <$!> VGM.replicate n' value
  where
    -- TODO: try this:
    -- !n' = until (>= n) (* 2) 2
    !n' = shiftL (bitCeil n) 1

-- | Creates a boxed segment tree.
{-# INLINE newSTreeV #-}
newSTreeV :: PrimMonad m => (a -> a -> a) -> Int -> a -> m (MSegmentTree VM.MVector (PrimState m) a)
newSTreeV = newSTreeVG

-- | Creates an unboxed segment tree.
{-# INLINE newSTreeVU #-}
newSTreeVU :: (VU.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree VUM.MVector (PrimState m) a)
newSTreeVU = newSTreeVG

-- | Sets all the internal values of a segment tree to the given value which has to be zero.
-- |
-- | REMARK: It takes lots of time. Consider a much more efficient resettiong strategy such as
-- | re-inserting zeros to used slots, or maybe use | `compressInvNumVG` when you just need
-- | inversion number.
resetSTree :: (VGM.MVector v a, PrimMonad m) => (MSegmentTree v (PrimState m) a) -> a -> m ()
resetSTree (MSegmentTree !_ !vec) !zero = VGM.set vec zero

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
{-# INLINE insertSTree #-}
insertSTree :: (VGM.MVector v a, PrimMonad m) => MSegmentTree v (PrimState m) a -> Int -> a -> m ()
insertSTree tree@(MSegmentTree !_ !vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = VGM.length vec `div` 2 - 1
    -- leaf index
    !i' = i + offset

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
{-# INLINE modifySTree #-}
modifySTree :: (VGM.MVector v a, PrimMonad m) => MSegmentTree v (PrimState m) a -> (a -> a) -> Int -> m ()
modifySTree tree@(MSegmentTree !_ !vec) !f !i = do
  !v <- f <$> VGM.unsafeRead vec i'
  _updateElement tree i' v
  where
    -- length == 2 * (the number of the leaves)
    !offset = VGM.length vec `div` 2 - 1
    -- leaf index
    !i' = i + offset

-- | (Internal) Updates an `MSegmentTree` element (node or leaf) value and their parents up to top root.
-- REMARK: It's faster to not INLINE the recursive function:
_updateElement :: (VGM.MVector v a, PrimMonad m) => MSegmentTree v (PrimState m) a -> Int -> a -> m ()
_updateElement (MSegmentTree !_ !vec) 0 !value = do
  VGM.unsafeWrite vec 0 value
_updateElement tree@(MSegmentTree !f !vec) !i !value = do
  VGM.unsafeWrite vec i value
  case ((i - 1) `div` 2) of
    -- REMARK: (-1) `div` 2 == -1
    -- TODO: This case never happens, right?
    (-1) -> return ()
    !iParent -> do
      !c1 <- VGM.unsafeRead vec (iParent * 2 + 1)
      !c2 <- VGM.unsafeRead vec (iParent * 2 + 2)
      _updateElement tree iParent (f c1 c2)

-- | Retrieves the folding result over the inclusive range `[l, r]` from `MSegmentTree`.
{-# INLINE querySTree #-}
querySTree :: forall v a m. (VGM.MVector v a, PrimMonad m) => MSegmentTree v (PrimState m) a -> (Int, Int) -> m (Maybe a)
querySTree (MSegmentTree !f !vec) (!lo, !hi)
  | lo > hi = return Nothing
  | otherwise = inner 0 (0, initialHi)
  where
    !initialHi = VGM.length vec `div` 2 - 1
    inner :: Int -> (Int, Int) -> m (Maybe a)
    inner !i (!l, !h)
      | lo <= l && h <= hi = Just <$> VGM.unsafeRead vec i
      | h < lo || hi < l = return Nothing
      | otherwise = do
          let !d = (h - l) `div` 2
          !ansL <- inner (2 * i + 1) (l, l + d)
          !ansH <- inner (2 * i + 2) (l + d + 1, h)
          pure . Just $ case (ansL, ansH) of
            (Just !a, Just !b) -> f a b
            (Just !a, _) -> a
            (_, Just !b) -> b
            (_, _) -> error $ "query error (segment tree): " ++ show (i, (l, h), (lo, hi))

-- }}}

-- {{{ Inveresion number (segment tree)

-- | Calculates the inversion number.
invNumVG :: Int -> (VG.Vector v Int) => v Int -> Int
invNumVG xMax xs = runST $ do
  !stree <- newSTreeVU (+) (xMax + 1) (0 :: Int)

  -- NOTE: foldM is better for performance
  !ss <- VG.forM xs $ \x -> do
    -- count pre-inserted numbers bigger than this:
    let !_ = dbg (x, (succ x, xMax))
    !s <-
      if x == xMax
        then return 0
        else fromJust <$> querySTree stree (succ x, xMax)

    -- let !_ = traceShow (x, s, (succ x, pred n)) ()
    modifySTree stree succ x
    return s

  return $ VG.sum ss

-- | Calculates the inversion number after applying index compression.
-- | It can significantly improve the performance, like in ABC 261 F.
compressInvNumVG :: VU.Vector Int -> Int
compressInvNumVG xs = invNumVG (pred (VU.length xs')) xs'
  where
    !xs' = snd $ compressVU xs

-- }}}

-- {{{ Dynamic programming

-- let dp = tabulateST f rng (0 :: Int)
--     rng = ((0, 0), (nItems, wLimit))
--     -- type signature can be inferred:
--     f :: forall s. MArray (STUArray s) Int (ST s) => STUArray s (Int, Int) Int -> (Int, Int) -> (ST s) Int
--     f _ (0, _) = return 0
--     f arr (i, w) = do

-- | REMARK: Very slow (somehow..). Maybe `Data.Ix` is not fast enough, or `f` is not inlined?
-- {-# INLINE tabulateST #-}
tabulateST :: forall i e. (Ix i, forall s. MArray (STUArray s) e (ST s)) => (forall s. STUArray s i e -> i -> ST s e) -> (i, i) -> e -> UArray i e
tabulateST f bounds_ e0 = runSTUArray uarray
  where
    uarray :: forall s. MArray (STUArray s) e (ST s) => ST s (STUArray s i e)
    uarray = do
      tbl <- newArray bounds_ e0 :: ST s (STUArray s i e)
      forM_ (range bounds_) $ \i -> do
        e <- f tbl i
        writeArray tbl i e
      return tbl

-- }}}

-- {{{ Dictionary orders

prevPermutationVec :: (Ord e, VG.Vector v e, VG.Vector v (Down e)) => v e -> v e
prevPermutationVec =
  VG.map (\case Down !x -> x)
    . VG.modify ((>> return ()) . VGM.nextPermutation)
    . VG.map Down

-- | Returns 1-based dictionary order for the given array.
-- | WARNING: Use 0-based indices for the input.
dictOrderModuloVec :: (VG.Vector v Int) => v Int -> Int -> Int
dictOrderModuloVec xs modulus = runST $ do
  !stree <- newSTreeVU (+) (VG.length xs + 1) (0 :: Int)

  -- Pre-calculate factorial numbers:
  let !facts = factMods (VG.length xs) modulus

  -- The calculation is very similar to that of inversion number. For example,
  -- ```
  --     2 0 4 3 1
  --     | | | | |
  --     | | | | +-- 0 * 0!
  --     | | | +-- 1 * 1!
  --     | | +-- 2 * 2!
  --     | +-- 0 * 3 !
  --     +-- 2 * 4!
  -- ```
  -- So each expression is given as `(the number of unused numbers smaller than this) * factMod`.
  !counts <- flip VG.imapM xs $ \i x -> do
    !nUsed <- fromJust <$> querySTree stree (0, x)
    let !nUnused = x - nUsed
    let !factMod = facts VG.! (VG.length xs - (i + 1))
    let !inc = nUnused * factMod `rem` modulus

    -- mark it as used
    insertSTree stree x 1

    return inc

  return $ succ $ VG.foldl1' (\ !acc x -> (acc + x) `rem` modulus) counts

-- }}}

-- {{{ Graph search

-- TODO: rewrite all

-- | Adjacency list representation of a graph with cost type parameter `a`.
type Graph a = Array Int [a]

-- | Weighted `Graph` (Entry priority payload).
type WGraph a = Array Int [H.Entry a Int]

dfsEveryVertex :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph Int -> Int -> s -> (s, IS.IntSet)
dfsEveryVertex (!isEnd, !fin, !fout) !graph !start !s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNode (!s, !visits) !x
      | isEnd s = (s, visits)
      | IS.member x visits = (s, visits)
      | otherwise =
          let (!s', !visits') = visitNeighbors (fin s x, IS.insert x visits) x
           in -- !_ = traceShow (start, x, graph ! x) ()
              (fout s' x, visits')

    visitNeighbors :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNeighbors (!s, !visits) !x
      | isEnd s = (s, visits)
      | otherwise = foldl' visitNode (s, visits) (graph ! x)

dfsEveryPath :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph Int -> Int -> s -> s
dfsEveryPath (!isEnd, !fin, !fout) !graph !start !s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> s
    visitNode (!s, !visits) !x
      | isEnd s = s
      | otherwise = flip fout x $ visitNeighbors (fin s x, IS.insert x visits) x

    visitNeighbors :: (s, IS.IntSet) -> Int -> s
    visitNeighbors (!s, !visits) !x
      | isEnd s = s
      | otherwise =
          foldl' (\ !s2 !n -> visitNode (s2, visits) n) s $ filter (`IS.notMember` visits) (graph ! x)

-- | Searches for a specific route in breadth-first order.
-- | Returns `Just (depth, node)` if succeed.
-- TODO: refactor / test it
bfsFind :: (Int -> Bool) -> Graph Int -> Int -> Maybe (Int, Int)
bfsFind !f !graph !start =
  if f start
    then Just (0, start)
    else bfsRec 1 (IS.singleton start) (IS.fromList $ graph ! start)
  where
    bfsRec :: Int -> IS.IntSet -> IS.IntSet -> Maybe (Int, Int)
    bfsRec !depth !visits !nbs
      | IS.null nbs = Nothing
      | otherwise =
          let -- !_ = traceShow ("bfsRec", depth, nbs) ()
              !visits' = IS.union visits nbs
           in let (!result, !nextNbs) = visitNeighbors visits' nbs
               in case result of
                    Just !x -> Just (depth, x)
                    Nothing -> bfsRec (succ depth) visits' nextNbs

    visitNeighbors :: IS.IntSet -> IS.IntSet -> (Maybe Int, IS.IntSet)
    visitNeighbors !visits !nbs =
      foldl'
        ( \(!result, !nbs') !x ->
            let nbs'' = IS.union nbs' $ IS.fromList . filter (`IS.notMember` visits) $ graph ! x
             in if f x
                  then (Just x, nbs'')
                  else (result, nbs'')
        )
        (Nothing, IS.empty)
        (IS.toList nbs)

dijkstra :: forall s. (s -> H.Entry Int Int -> s) -> s -> WGraph Int -> Int -> s
dijkstra !f !s0 !graph !start = fst3 $! visitRec (s0, IS.empty, H.singleton $! H.Entry 0 start)
  where
    visitRec :: (s, IS.IntSet, H.Heap (H.Entry Int Int)) -> (s, IS.IntSet, H.Heap (H.Entry Int Int))
    visitRec (!s, !visits, !heap) =
      case H.uncons heap of
        Just (!x, !heap') ->
          if IS.member (H.payload x) visits
            then visitRec (s, visits, heap')
            else visitRec $ visitNode (s, visits, heap') x
        Nothing -> (s, visits, heap)

    visitNode :: (s, IS.IntSet, H.Heap (H.Entry Int Int)) -> H.Entry Int Int -> (s, IS.IntSet, H.Heap (H.Entry Int Int))
    visitNode (!s, !visits, !heap) entry@(H.Entry cost x) =
      let !visits' = IS.insert x visits
          !news = H.fromList . map (first (cost +)) . filter p $ graph ! x
          !p = not . (`IS.member` visits') . H.payload
       in (f s entry, visits', H.union heap news)

-- }}}

-- {{{ Digraph

-- | Red | Green color
type Color = Bool

-- | Colored vertices in a bipartite graph
type ColorInfo = ([Int], [Int])

-- | DFS with vertices given colors
colorize :: Graph Int -> IM.IntMap Color -> G.Vertex -> (IM.IntMap Color, Maybe ColorInfo)
colorize !graph !colors0 = dfs True (colors0, Just ([], []))
  where
    dfs :: Color -> (IM.IntMap Color, Maybe ColorInfo) -> G.Vertex -> (IM.IntMap Color, Maybe ColorInfo)
    dfs !color (!colors, !acc) !v =
      let (!colors', !acc') = setColor color (colors, acc) v
       in if IM.member v colors
            then (colors', acc')
            else foldl' (dfs (not color)) (colors', acc') $ graph ! v

    setColor :: Color -> (IM.IntMap Color, Maybe ColorInfo) -> G.Vertex -> (IM.IntMap Color, Maybe ColorInfo)
    setColor !color (!colors, !acc) !v =
      case IM.lookup v colors of
        Just c
          | c == color -> (colors, acc)
          | otherwise -> (colors, Nothing)
        Nothing -> (IM.insert v color colors, applyColor color v acc)

    applyColor :: Color -> G.Vertex -> Maybe ColorInfo -> Maybe ColorInfo
    applyColor !_ !_ Nothing = Nothing
    applyColor !color !v (Just !acc)
      | color = Just $ first (v :) acc
      | otherwise = Just $ second (v :) acc

-- }}}

-- {{{ Topological sort / SCC

-- | Topological sort implemented with postorder DFS.
-- |
-- | # Implementation note
-- | Topological sort is for DAG, but internally it's used for `scc` where asyclic graph input can
-- | come.
topSort :: Array Int [Int] -> [Int]
topSort !graph = runST $ do
  let !bounds_ = bounds graph
  !vis <- VUM.replicate (succ $ rangeSize bounds_) False

  let dfsM !acc !v = do
        !b <- VUM.unsafeRead vis (index bounds_ v)
        if b
          then return acc
          else do
            VUM.unsafeWrite vis (index bounds_ v) True
            !vs <- filterM (fmap not . VUM.unsafeRead vis . index bounds_) $ graph ! v
            -- Create postorder output:
            (v :) <$> foldM dfsM acc vs

  foldM dfsM [] $ range bounds_

-- | Partial running of `scc` over topologically sorted vertices, but for sone connected components
-- | only.
topScc1 :: forall m. (PrimMonad m) => Array Int [Int] -> VUM.MVector (PrimState m) Bool -> Int -> m [Int]
topScc1 !graph' !vis !v0 = do
  let !bounds_ = bounds graph'

  let dfsM !acc !v = do
        !b <- VUM.unsafeRead vis (index bounds_ v)
        if b
          then return acc
          else do
            VUM.unsafeWrite vis (index bounds_ v) True
            !vs <- filterM (fmap not . VUM.unsafeRead vis . index bounds_) $ graph' ! v
            -- Create preorder output:
            (v :) <$> foldM dfsM acc vs

  dfsM [] v0

-- | Retrieves a reverse graph
revGraph :: Array Int [Int] -> Array Int [Int]
revGraph graph = accumArray (flip (:)) [] (bounds graph) input
  where
    input :: [(Int, Int)]
    input = foldl' (\ !acc (!v2, !v1s) -> foldl' (\ !acc' !v1 -> (v1, v2) : acc') acc v1s) [] $ assocs graph

-- | Collectes strongly connected components, topologically sorted.
topScc :: Array Int [Int] -> [[Int]]
topScc graph = collectSccPreorder $ topSort graph
  where
    graph' = revGraph graph

    collectSccPreorder :: [Int] -> [[Int]]
    collectSccPreorder !topVerts = runST $ do
      let !bounds_ = bounds graph'
      !vis <- VUM.replicate (succ $ rangeSize bounds_) False
      filter (not . null) <$> mapM (topScc1 graph' vis) topVerts

-- | Collects cycles using `scc`.
topSccCycles :: Array Int [Int] -> [[Int]]
topSccCycles graph = filter f $ topScc graph
  where
    -- self-referencial loop only
    f [!v] = [v] == graph ! v
    f !_ = True

-- }}}

-- {{{ Graph search (V2)

-- | Collects distances from one vertex to every other using BFS, returning a vector.
bfsVec :: Graph Int -> Int -> VU.Vector Int
bfsVec graph start = VU.create $ do
  let !undef = -1 :: Int
  !vis <- VUM.replicate (rangeSize $ bounds graph) undef

  let inner !depth !vs
        | IS.null vs = return ()
        | otherwise = do
            let vs' = IS.toList vs
            forM_ vs' $ \v -> do
              VUM.unsafeWrite vis v depth

            !vss <- forM vs' $ \v -> do
              filterM (\v2 -> (== undef) <$> VUM.unsafeRead vis v2) $ graph ! v

            inner (succ depth) $ IS.fromList $ concat vss

  !_ <- inner (0 :: Int) (IS.singleton start)
  return vis

-- | BFS template for finding a shortest path from one vertex to another.
bfsPath :: Graph Int -> Int -> Int -> Maybe Int
bfsPath !graph !start !end = inner (-1) IS.empty (IS.singleton start)
  where
    inner :: Int -> IS.IntSet -> IS.IntSet -> Maybe Int
    inner !depth !vis !vs
      | IS.member end vis = Just depth
      | IS.null vs = Nothing
      | otherwise = inner (succ depth) vis' vs'
      where
        vis' = vis `IS.union` vs
        vs' = IS.fromList $! filter (`IS.notMember` vis') $! concatMap (graph !) (IS.toList vs)

-- | BFS template for collecting shortest paths from one vertex to every other.
bfsVerts :: Graph Int -> Int -> IM.IntMap Int
bfsVerts graph start = inner 0 IM.empty (IS.singleton start)
  where
    inner :: Int -> IM.IntMap Int -> IS.IntSet -> IM.IntMap Int
    inner !depth !vis !vs
      | IS.null vs = vis
      | otherwise = inner (succ depth) vis' vs'
      where
        vis' = IM.union vis $! IM.fromSet (const depth) vs
        vs' = IS.fromList $! filter (`IM.notMember` vis') $! concatMap (graph !) (IS.toList vs)

-- | BFS over grid. Not generalized (yet).
bfsGrid :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Int
bfsGrid !grid !start = runSTUArray $ do
  let bounds_ = bounds grid
  let (!h, !w) = both succ $ snd bounds_
  let isBlock !yx = grid ! yx == '#'

  let ix = index bounds_
  let unIndex !i = i `divMod` w
  let !undef = -1 :: Int

  !vis <- newArray bounds_ undef

  let nexts !yx0 = filter (\yx -> inRange bounds_ yx && not (isBlock yx)) $ map (add2 yx0) dyxs
        where
          dyxs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

  let inner !depth !vs
        | IS.null vs = return ()
        | otherwise = do
            let yxs = map unIndex $ IS.toList vs

            forM_ yxs $ \yx -> do
              writeArray vis yx depth

            !vss <- forM yxs $ \yx -> do
              filterM (\yx2 -> (== undef) <$> readArray vis yx2) $ nexts yx

            inner (succ depth) $ IS.fromList . map ix $ concat vss

  !_ <- inner (0 :: Int) (IS.singleton $ ix start)
  return vis

-- 01-BFS: <https://atcoder.jp/contests/typical90/tasks/typical90_aq>
-- It's slow, but could be applied easily.
bfsGrid01 :: (Int, Int) -> UArray (Int, Int) Bool -> UArray (Int, Int, Int) Int
bfsGrid01 !start !isBlock = runSTUArray $ do
  -- dp ! (y, x, iDir). The third dimension is required!
  !dp <- newArray ((0, 0, 0), (pred h, pred w, pred 4)) undef

  forM_ [0 .. 3] $ \iDir -> do
    writeArray dp (fst start, snd start, iDir) 0

  let popLoop Seq.Empty = return ()
      popLoop (((!y0, !x0, !iDir0), d0) Seq.:<| seq0) =
        foldM step seq0 [0 .. 3] >>= popLoop
        where
          -- collects neighbors
          step !acc !iDir
            | not (inRange bounds_ (y, x)) || isBlock ! (y, x) = return acc
            | otherwise = do
                !lastD <- readArray dp (y, x, iDir)
                -- REMARK: we can come to the same point in the same direction in different ways:
                if lastD /= undef && lastD <= d'
                  then return acc
                  else do
                    writeArray dp (y, x, iDir) d'
                    if iDir == iDir0
                      then return $ nextItem Seq.<| acc
                      else return $ acc Seq.|> nextItem
            where
              (!y, !x) = add2 (y0, x0) (dyxs VU.! iDir)
              !d'
                | iDir == iDir0 = d0
                | otherwise = succ d0
              !nextItem = ((y, x, iDir), d')

  popLoop . Seq.fromList $ map (\iDir -> ((fst start, snd start, iDir), 0)) [0 .. 3]
  return dp
  where
    !undef = -1 :: Int
    !bounds_ = bounds isBlock
    (!h, !w) = both succ . snd $ bounds isBlock
    !dyxs = VU.fromList $ [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- | DFS where all the reachable vertices from one vertex are collcetd.
components :: Graph Int -> Int -> IS.IntSet
components !graph !start = inner (IS.singleton start) start
  where
    inner vis v
      | null vs = vis'
      | otherwise = foldl' inner vis' vs
      where
        vs = filter (`IS.notMember` vis) $! graph ! v
        vis' = IS.union vis $! IS.fromList vs

-- | Dijkstra template that collects all the shortest distances from one vertex to every other.
-- | Works for weightened graphs with positive edge capacities only.
-- |
-- | Pro tip: Use reverse graph to collect cost from every other vertex to one (see `revDjAll`).
dj :: forall a. (Num a, Ord a) => WGraph a -> Int -> IM.IntMap a
dj !graph !start = inner (H.singleton $! H.Entry 0 start) IM.empty
  where
    merge :: H.Entry a Int -> H.Entry a Int -> H.Entry a Int
    merge (H.Entry !cost1 !_v1) (H.Entry !cost2 !v2) = H.Entry (cost1 + cost2) v2

    inner :: H.Heap (H.Entry a Int) -> IM.IntMap a -> IM.IntMap a
    inner !heap !vis
      | H.null heap = vis
      | IM.member v vis = inner heap' vis
      | otherwise = inner heap'' vis'
      where
        -- pop and visit it
        (entry@(H.Entry cost v), heap') = fromJust $! H.uncons heap
        vis' = IM.insert v cost vis

        -- push neighbors
        vs = map (merge entry) $! filter ((`IM.notMember` vis') . H.payload) $! graph ! v
        heap'' = foldl' (flip H.insert) heap' vs

-- | Runs dijkstra's algorithm over a reversed graph of given graph.
-- | It calculates
revDj :: WGraph Int -> Int -> IM.IntMap Int
revDj !graph !start =
  let !graph' = revWGraph graph
   in dj graph' start

revWGraph :: WGraph Int -> WGraph Int
revWGraph !graph = accumArray @Array (flip (:)) [] (bounds graph) $ concatMap revF $ assocs graph
  where
    revF (!v1, !v2s) = map (\(H.Entry !priority !v2) -> (v2, H.Entry priority v1)) v2s

djVec :: forall a. (Num a, Ord a, VU.Unbox a) => WGraph a -> Int -> a -> VU.Vector a
djVec !graph !start !undef = VU.create $ do
  !vis <- VUM.replicate nVerts undef

  let inner !heap = case H.uncons heap of
        Nothing -> return ()
        Just (entry@(H.Entry cost v), heap') -> do
          !isNew <- (== undef) <$> VUM.read vis v
          if not isNew
            then inner heap'
            else do
              VUM.write vis v cost
              !vs <- map (merge entry) <$> (filterM (fmap (== undef) . VUM.read vis . H.payload) $ graph ! v)
              inner $ foldl' (flip H.insert) heap' vs

  inner (H.singleton $ H.Entry 0 start)
  return vis
  where
    !nVerts = rangeSize $ bounds graph

    merge :: H.Entry a Int -> H.Entry a Int -> H.Entry a Int
    merge (H.Entry !cost1 !_v1) (H.Entry !cost2 !v2) = H.Entry (cost1 + cost2) v2

-- }}}

-- {{{ Tree

-- Returns `(parents, depths)` who maps vertices to the corresponding information.
-- REMARK: Use 0-based index for the graph vertices.
treeDepthInfo :: Graph Int -> Int -> (VU.Vector Int, VU.Vector Int)
treeDepthInfo !graph !root = runST $ do
  !parents <- VUM.replicate nVerts (-1 :: Int)
  !depths <- VUM.replicate nVerts (-1 :: Int)

  let m (!depth, !parent, !vs) = do
        forM_ vs $ \v -> do
          VUM.unsafeWrite depths v depth
          VUM.unsafeWrite parents v parent
          let !vs' = filter (/= parent) $ graph ! v
          m (succ depth, v, vs')

  !_ <- m (0 :: Int, -1 :: Int, [root])

  (,) <$> VU.unsafeFreeze parents <*> VU.unsafeFreeze depths
  where
    !nVerts = rangeSize $ bounds graph

-- | Returns `(parents, depths, doubling)` two of which can be used for `lca`.
lcaCache :: Graph Int -> Int -> (VU.Vector Int, VU.Vector Int, V.Vector (VU.Vector Int))
lcaCache graph root = (parents, depths, doubling)
  where
    (!parents, !depths) = treeDepthInfo graph root
    !doubling = newDoubling parents $ \acc -> VU.map (\case -1 -> -1; i -> acc VU.! i) acc

-- Returns the lowest common ancestor `(v, d)` with the help of doubling technique.
-- REMARK: Use 0-based index for the graph vertices.
lca :: VU.Vector Int -> V.Vector (VU.Vector Int) -> Int -> Int -> (Int, Int)
lca !depths !doubling !v1 !v2 = (vLCA, depths VU.! vLCA)
  where
    -- depths
    !d1 = depths VU.! v1
    !d2 = depths VU.! v2

    -- go up N depths
    parentN !v !n =
      applyDoubling
        doubling
        v
        ( \v' mapper -> case v' of
            (-1) -> -1
            _ -> mapper VU.! v'
        )
        n

    -- v1' and v2' are of the same depth
    !v1' = if d1 <= d2 then v1 else v2
    !v2' = parentN (if d1 > d2 then v1 else v2) (abs $ d1 - d2)

    -- go up `dLCA` depth to find the lca
    !dLCA = fromJust . snd $ bsearch (0, min d1 d2) \d ->
      let !p1 = parentN v1' d
          !p2 = parentN v2' d
       in p1 /= p2

    !vLCA = parentN v1' dLCA

-- | Gets the length between given two vertices with the help of LCA.
lcaLen :: VU.Vector Int -> V.Vector (VU.Vector Int) -> Int -> Int -> Int
lcaLen !depths !doubling !v1 !v2 =
  let (!_, !d) = lca depths doubling v1 v2
      !d1 = depths VU.! v1
      !d2 = depths VU.! v2
   in (d1 - d) + (d2 - d)

-- | Folds a tree from one root vertex using postorder DFS.
foldTree :: Array Int [Int] -> Int -> m -> (m -> m -> m) -> m
foldTree !tree !root !acc0 !mact = inner (-1) root
  where
    inner !parent !v1 =
      let !v2s = filter (/= parent) $ tree ! v1
       in foldl' (\acc v2 -> acc `mact` (inner v1 v2)) acc0 v2s

-- | Folds a tree from one root vertex using postorder DFS, recording all the accumulation values
-- | on every vertex.
scanTreeVG :: (VG.Vector v m) => Array Int [Int] -> Int -> m -> (m -> m -> m) -> v m
scanTreeVG !tree !root !acc0 !mact = VG.create $ do
  !dp <- VGM.unsafeNew nVerts
  !_ <- flip fix (-1, root) $ \runTreeDp (!parent, !v1) -> do
    let !v2s = filter (/= parent) $ tree ! v1
    !x1 <- foldM (\acc v2 -> (acc `mact`) <$> runTreeDp (v1, v2)) acc0 v2s
    VGM.write dp v1 x1
    return x1

  return dp
  where
    !nVerts = rangeSize $ bounds tree

scanTreeVU :: VU.Unbox m => Array Int [Int] -> Int -> m -> (m -> m -> m) -> VU.Vector m
scanTreeVU = scanTreeVG

-- | Folds a tree for every vertex as a root using the rerooting technique.
-- | Also known as tree DP with rerooting.
-- Add `Show m` for debug.
foldTreeAll :: VU.Unbox m => Array Int [Int] -> (OperatorMonoid m m) -> m -> (VU.Vector m)
foldTreeAll !tree (OperatorMonoid !op0 !mapp !mact) !acc0 =
  -- Calculate tree DP for one root vertex
  let !treeDp = scanTreeVG tree root0 acc0 mact
      !rootDp = VU.create $ do
        -- Calculate tree DP for every vertex as a root:
        !dp <- VUM.unsafeNew nVerts
        flip fix (-1, op0, root0) $ \runRootDp (!parent, !parentOp, !v1) -> do
          let !children = VU.fromList . filter (/= parent) $ tree ! v1
          let !opL = VU.scanl' (\op v2 -> (op `mapp`) $ treeDp VU.! v2) op0 children
          let !opR = VU.scanr (\v2 op -> (op `mapp`) $ treeDp VU.! v2) op0 children

          -- save
          let !x1 = acc0 `mact` (parentOp `mapp` (VU.last opL))
          VUM.write dp v1 x1
          -- let !_ = dbg ("dfs", (parent, parentOp), "->", (v1, x1), children, opR)

          flip VU.imapM_ children $ \ !i2 !v2 -> do
            let !lrOp = (opL VU.! i2) `mapp` (opR VU.! succ i2)
            -- REMARK: We assume the accumulated value has information to be used as an operator:
            let !v1Acc = acc0 `mact` (parentOp `mapp` lrOp)
            runRootDp (v1, v1Acc, v2)

        return dp
   in rootDp
  where
    !nVerts = rangeSize $ (bounds tree)
    !root0 = 0 :: Int

-- }}}

-- {{{ Minimum spanning tree (Kruskal's algorithm)

-- Find a minimum spanning tree by eagerly adding the lightest path

-- TODO: add template

-- }}}

-- {{{ Every shortest path (Floyd-Warshall algorithm)

-- Get the shortest path between every pair of the vertices in a weightend graph

-- | Create buffer for the Floyd-Warshapp algorithm
{-# INLINE newFW #-}
newFW :: (PrimMonad m, VU.Unbox cost) => (G.Vertex -> cost, cost, cost) -> Int -> [(Int, Int)] -> m (VUM.MVector (PrimState m) cost)
newFW (!getCost, !zeroCost, !maxCost) !nVerts !edges = do
  -- REMARK: Boxed array is too slow
  !dp <- VUM.replicate (nVerts * nVerts) maxCost

  -- diagnonal components
  forMS_ (rangeMS 0 (pred nVerts)) $ \ !v ->
    VUM.unsafeWrite dp (ix (v, v)) zeroCost

  -- directly connected vertices
  forM_ edges $ \(!v1, !v2) -> do
    -- let !_ = traceShow (v1, v2, values VU.! v2) ()
    -- (distance, value)
    let !cost = getCost v2
    VUM.unsafeWrite dp (ix (v1, v2)) cost

  return dp
  where
    ix :: (Int, Int) -> Int
    ix = index ((0, 0), (nVerts - 1, nVerts - 1))

{-# INLINE runFW #-}
runFW :: (PrimMonad m, VU.Unbox cost) => (cost -> cost -> cost, cost -> cost -> cost) -> Int -> VUM.MVector (PrimState m) cost -> m ()
runFW (!mergeCost, !minCost) !nVerts !dp = do
  let !ve = pred nVerts
  forM_ (range ((0, 0, 0), (ve, ve, ve))) $ \(!v3, !v1, !v2) -> do
    !cost1 <- VUM.unsafeRead dp (ix (v1, v2))
    !cost2 <- mergeCost <$> VUM.unsafeRead dp (ix (v1, v3)) <*> VUM.unsafeRead dp (ix (v3, v2))
    -- let !_ = traceShow ((v3, v2, v1), cost1, cost2, mergeCost cost1 cost2) ()
    VUM.unsafeWrite dp (ix (v1, v2)) $ minCost cost1 cost2
  where
    ix :: (Int, Int) -> Int
    ix = index ((0, 0), (nVerts - 1, nVerts - 1))

-- Floyd-Warshall algorithm over `WGraph`
-- TODO: test it
-- newFW_W :: (G.Vertex -> Int) -> Int -> [(Int, Int)] -> IO (VUM.IOVector Int)
-- newFW_W getCost = newFW (getCost, 0 :: Int, maxBound @Int)

--  Floyd-Warshall algorithm over `Graph` + vertex values (see ABC 286 E)
{-# INLINE newFW_ABC286E #-}
newFW_ABC286E :: (PrimMonad m) => (G.Vertex -> (Int, Int)) -> Int -> [(Int, Int)] -> m (VUM.MVector (PrimState m) (Int, Int))
newFW_ABC286E !getCost = newFW (getCost, (0, 0), (maxBound @Int, maxBound @Int))

{-# INLINE runFW_ABC286E #-}
runFW_ABC286E :: (PrimMonad m) => Int -> VUM.MVector (PrimState m) (Int, Int) -> m ()
runFW_ABC286E = runFW (mergeCost, minCost)
  where
    mergeCost :: (Int, Int) -> (Int, Int) -> (Int, Int)
    mergeCost (!d1, !v1) (!d2, !v2)
      -- if not connected (TODO: use `Maybe` instead of `maxBound`
      | d1 == maxBound = (d1, v1)
      | d2 == maxBound = (d2, v2)
      -- if connected
      | d1 == maxBound = (d1, v1)
      | otherwise = (d1 + d2, v1 + v2)

    minCost :: (Int, Int) -> (Int, Int) -> (Int, Int)
    minCost (!d1, !v1) (!d2, !v2) =
      case compare d1 d2 of
        EQ -> (d1, max v1 v2)
        LT -> (d1, v1)
        GT -> (d2, v2)

-- }}}

-- {{{ Maximum flow (Ford-Fulkerson algorithm)

-- Find the maximum flow from one vertex to another by repeatedly finding augument path and teaking
-- the flow.

-- TODO: Use `ST` monad for the visit buffer.. but how? (ST monad transformer??)

-- | Edge in residual network from on vertex to another.
data RNEdge = RNEdge
  { -- | Points the the other side of the edge
    to :: {-# UNPACK #-} !G.Vertex,
    -- | Capacity of the edge, or the flow from the vertex to another
    cap :: {-# UNPACK #-} !Int,
    -- | The other side of the vertices is pointed with `rn ! (rev (rn ! to))`
    -- | so that edge insertion takes just $O(1)$.
    rev :: {-# UNPACK #-} !Int
  }
  deriving (Show)

derivingUnbox
  "RNEdge"
  [t|RNEdge -> (G.Vertex, Int, Int)|]
  [|\(RNEdge !x1 !x2 !x3) -> (x1, x2, x3)|]
  [|\(!x1, !x2, !x3) -> RNEdge x1 x2 x3|]

-- | `Vertex` -> `[RNEdge]`
-- TODO: For the sub containers, use `Sequence` or something better
type ResidualNetwork = VM.IOVector (IM.IntMap RNEdge)

-- | Builds a residual network at initial state from given edges `(v2, cost)`.
-- {-# INLINE buildRN #-}
-- TODO: make it generic over ST.. for no reason?
buildRN :: Int -> [(Int, (Int, Int))] -> IO ResidualNetwork
buildRN !nVerts !edges = do
  !rn <- VM.replicate nVerts IM.empty
  -- TODO: consider using `VU.accumlate` instead?
  forM_ edges $ \(!v1, (!v2, !cap_)) -> do
    addEdgeRN rn v1 v2 cap_
  return rn
  where
    addEdgeRN :: ResidualNetwork -> Int -> Int -> Int -> IO ()
    addEdgeRN !rn !v1 !v2 !maxFlow = do
      !edges1 <- VM.read rn v1
      !edges2 <- VM.read rn v2

      -- REMARK: Be sure to use `insertWith`!
      -- We can have both (v1 -> v2) path and (v2 -> v1) path

      -- We can run up to `maxFlow`:
      VM.write rn v1 $ IM.insertWith mergeEdge v2 (RNEdge v2 maxFlow v1) edges1
      -- We cannot reverse when there's no flow:
      VM.write rn v2 $ IM.insertWith mergeEdge v1 (RNEdge v1 0 v2) edges2

    mergeEdge :: RNEdge -> RNEdge -> RNEdge
    mergeEdge (RNEdge !to_ !flow !cap_) (RNEdge !_ !flow' !_) = RNEdge to_ (flow + flow') cap_

{-# INLINE maxFlowRN #-}
maxFlowRN :: Int -> ResidualNetwork -> Int -> Int -> IO Int
maxFlowRN !nVerts !rn !v0 !ve = do
  -- TODO: use BitVec in 2023 environment
  !vis <- VM.replicate nVerts False
  inner vis
  where
    inner :: VM.IOVector Bool -> IO Int
    inner !vis =
      augumentPath rn vis v0 ve >>= \case
        Nothing -> return 0
        Just (!flow, !path) -> do
          updateFlow rn flow path
          VM.set vis False
          (flow +) <$!> inner vis

-- | Find a flow augment path between two vertices.
{-# INLINE augumentPath #-}
augumentPath :: ResidualNetwork -> VM.IOVector Bool -> G.Vertex -> Int -> IO (Maybe (Int, [(G.Vertex, G.Vertex)]))
augumentPath !rn !vis !v0 !goal = visitVertex v0 (maxBound @Int)
  where
    visitVertex :: G.Vertex -> Int -> IO (Maybe (Int, [(G.Vertex, G.Vertex)]))
    visitVertex !v !flow
      | v == goal = return $ Just (flow, [])
      | otherwise = do
          VM.write vis v True
          !edges <- VM.read rn v
          foldM (step v flow) Nothing edges

    step :: G.Vertex -> Int -> Maybe (Int, [(G.Vertex, G.Vertex)]) -> RNEdge -> IO (Maybe (Int, [(G.Vertex, G.Vertex)]))
    step !_ !_ r@(Just _) _ = return r
    step !from !flow !_ !edge = do
      !visited <- VM.read vis (to edge)
      if visited || flow' == 0
        then return Nothing
        else
          visitVertex (to edge) flow' >>= \case
            Nothing -> return Nothing
            Just (!f, !path) -> return $ Just (f, p : path)
      where
        flow' = min flow (cap edge)
        p = (from, to edge)

{-# INLINE updateFlow #-}
updateFlow :: ResidualNetwork -> Int -> [(G.Vertex, G.Vertex)] -> IO ()
updateFlow !rn !flow !path = forM_ path $ \(!v1, !v2) -> addFlowRNEdge rn v1 v2 flow

{-# INLINE addFlowRNEdge #-}
addFlowRNEdge :: ResidualNetwork -> G.Vertex -> G.Vertex -> Int -> IO ()
addFlowRNEdge !rn !v1 !v2 !flow = do
  -- TODO: consider using `VM.modify`
  -- TODO: consider using `lens`, `snd2` (or not)
  -- TODO: replace `dupe` with function applicative?
  (!edges1, !edge12) <- second (IM.! v2) . dupe <$!> VM.read rn v1
  (!edges2, !edge21) <- second (IM.! v1) . dupe <$!> VM.read rn v2
  -- let !_ = traceShow ("edge", "v1:", v1, edge12, "v2:", v2, edge21, flow) ()

  -- TODO: debugAssert
  -- when (cap edge12 < flow) $ error "invariant broken"
  VM.write rn v1 $! IM.insert v2 (RNEdge (to edge12) (cap edge12 - flow) (rev edge12)) edges1
  VM.write rn v2 $! IM.insert v1 (RNEdge (to edge21) (cap edge21 + flow) (rev edge21)) edges2

-- }}}

-- {{{ Adhoc code

data MyModulus = MyModulus

instance TypeInt MyModulus where
  -- typeInt _ = 998244353
  typeInt _ = 1_000_000_007

type MyModInt = ModInt MyModulus

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` typeInt (Proxy @MyModulus))

undef :: Int
undef = -1

-- }}}

main :: IO ()
main = do
  [n] <- ints
  !xs <- intsVU

  putStrLn "TODO"

