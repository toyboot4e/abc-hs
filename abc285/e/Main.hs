#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers
-}

{- TODOs

- [ ] More templates
  - [ ] Graph
    - [ ] connections, scc, cycles
    - [ ] try minimum cut problem
  - [ ] Better, easier rolling hash

- [ ] Green steak

- [ ] More graph
  - [ ] Dijkstra2
  - [ ] Tessoku graph B

- [ ] DP
  - [ ] Tessoku DP
  - [ ] EDCP

- [ ] More practices
  - [ ] Chokudai Speedrun001, 002
  - [ ] Tessoku A71~, C11~
  - [ ] Typical 90
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, DefaultSignatures, LambdaCase, MultiWayIf #-}
{-# LANGUAGE NumDecimals, NumericUnderscores, PatternGuards, TupleSections #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE StrictData, TypeApplications, TypeFamilies, RankNTypes #-}

-- TODO: ditch `vector-th-unbox` and `TemplateHaskell` in 2023 environment
{-# LANGUAGE TemplateHaskell #-}
{- ORMOLU_ENABLE -}

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
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import Debug.Trace
import GHC.Event (IOCallback)
import GHC.Exts
import GHC.Float (int2Float)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

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
import Control.Monad.Extra -- foldM, ..
import Data.IORef.Extra    -- writeIORef'
import Data.List.Extra     -- merge, nubSort, ..
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

-- {{{ Prelude utilities

-- | From more recent GHC
clamp :: (Ord a) => (a, a) -> a -> a
clamp (!low, !high) !a = min high (max a low)

flipOrder :: Ordering -> Ordering
flipOrder = \case
  GT -> LT
  LT -> GT
  EQ -> EQ

-- }}}

-- {{{ Libary complements

{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray ary i f = do
  !v <- f <$> readArray ary i
  writeArray ary i v

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange !i !j = VU.enumFromN i (j + 1 - i)

-- NOTE: We can only lookup by priority (cost), not by payload (vertex)
lookupHeapEntry :: Int -> H.Heap (H.Entry Int Int) -> Maybe (H.Entry Int Int)
lookupHeapEntry !key !heap =
  let !h = H.intersect heap (H.singleton $ H.Entry key (0 :: Int))
   in if' (H.null h) Nothing $ Just (H.minimum h)

-- }}}

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- compress duduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

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

prevPermutationVec :: (Ord e, VG.Vector v e, VG.Vector v (Down e)) => v e -> v e
prevPermutationVec =
  VG.map (\case Down !x -> x)
    . VG.modify
      ( \ !vec -> do
          _ <- VGM.nextPermutation vec
          return ()
      )
    . VG.map Down

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

getTuple2 :: IO (Int, Int)
getTuple2 = tuple2 <$> getLineIntList

getTuple3 :: IO (Int, Int, Int)
getTuple3 = tuple3 <$> getLineIntList

getTuple4 :: IO (Int, Int, Int, Int)
getTuple4 = tuple4 <$> getLineIntList

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

-- }}}

-- {{{ Input

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | Creates a graph from 1-based vertices
getGraph :: Int -> Int -> IO (Array Int [Int])
getGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges getLineIntList
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ second swap . dupe . tuple2

-- | Creates a weightend graph from 1-based vertices
getWGraph :: Int -> Int -> IO (Array Int [H.Entry Int Int])
getWGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges getLineIntList
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ \[!a, !b, !cost] -> ((a, H.Entry cost b), (b, H.Entry cost a))

-- }}}

-- {{{ Output

putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

printBSB :: ShowBSB a => a -> IO ()
printBSB = putBSB . showBSB

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
unDigits !base = foldl' (\ !a  !b -> a * base + b) 0

-- | <https://stackoverflow.com/questions/10028213/converting-number-base>
-- | REMARK: It returns `[]` when giben `[0]`. Be sure to convert `[]` to `[0]` if necessary.
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
    msb = log2 x
    ceiling_ = if clearBit x msb > 0 then 1 else 0

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

-- @gotoki_no_joe
primes :: [Int]
primes = 2 : 3 : sieve q0 [5, 7 ..]
  where
    q0 = H.insert (H.Entry 9 6) H.empty
    sieve queue xxs@(x : xs) =
      case compare np x of
        LT -> sieve queue1 xxs
        EQ -> sieve queue1 xs
        GT -> x : sieve queue2 xs
      where
        H.Entry np p2 = H.minimum queue
        queue1 = H.insert (H.Entry (np + p2) p2) $ H.deleteMin queue
        queue2 = H.insert (H.Entry (x * x) (x * 2)) queue
    sieve _ _ = error "unreachale"

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
powerModCache !base !modulo = (modulo, VU.fromList $ scanl' (\ !x _ -> x * x `rem` modulo) base [(1 :: Int) .. 62])

-- | Calculates base^i (mod p) from a cache
powerByCache :: Int -> (Int, VU.Vector Int) -> Int
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

-- | nCr `mod` m (binominal cofficient)
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulus = foldl' (\ !x !y -> divModF x y modulus) (facts VU.! n) [facts VU.! r, facts VU.! (n - r)]
  where
    facts = VU.scanl' (\ !x !y -> x * y `rem` modulus) (1 :: Int) $ VU.fromList [(1 :: Int) .. 1_000_000]

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

-- | Binary search for sorted items in an inclusive range (from left to right only)
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

-- | Monadic variant of `bsearch`
bsearchM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bsearchM (!low, !high) !isOk = both wrap <$> inner (low - 1, high + 1)
  where
    inner :: (Int, Int) -> m (Int, Int)
    inner (!ok, !ng)
      | abs (ok - ng) == 1 = return (ok, ng)
      | otherwise =
        isOk m >>= \ !yes ->
          if yes
            then inner (m, ng)
            else inner (ok, m)
      where
        m = (ok + ng) `div` 2

    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

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
      | x == (low - diff) || x == (low + diff) = Nothing
      | otherwise = Just x

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
      | x == (low - diff) || x == (low + diff) = Nothing
      | otherwise = Just x

-- }}}

-- {{{ Dense, mutable union-Find tree

-- | Dense, mutable union-find tree (originally by `@pel`)
newtype MUnionFind s = MUnionFind (VUM.MVector s MUFNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | `MUFChild parent | MUFRoot size`. Not `Unbox` :(
data MUFNode = MUFChild {-# UNPACK #-} !Int | MUFRoot {-# UNPACK #-} !Int

derivingUnbox "MUFNode"
  [t|MUFNode -> (Bool, Int)|]
  [|\case (MUFChild !x) -> (True, x)  ; (MUFRoot !x) -> (False, x)|]
  [|\case (True, !x) -> MUFChild x; (False, !x) -> MUFRoot x|]

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF !n = MUnionFind <$> VUM.replicate n (MUFRoot 1)

-- | Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
rootMUF uf@(MUnionFind !vec) i = do
  !node <- VUM.read vec i
  case node of
    MUFRoot _ -> return i
    MUFChild p -> do
      !r <- rootMUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VUM.write vec i (MUFChild r)
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
    !sx <- _unwrapMUFRoot <$!> VUM.read vec px
    !sy <- _unwrapMUFRoot <$!> VUM.read vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (!par, !chld) = if sx < sy then (px, py) else (py, px)
    VUM.write vec chld (MUFChild par)
    VUM.write vec par (MUFRoot (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind !vec) !x = do
  !px <- rootMUF uf x
  _unwrapMUFRoot <$!> VUM.read vec px

-- }}}

-- {{{ Sparse, immutable union-find tree

-- @gotoki_no_joe
type SparseUnionFind = IM.IntMap Int

newSUF :: SparseUnionFind
newSUF = IM.empty

rootSUF :: SparseUnionFind -> Int -> (Int, Int)
rootSUF !uf !i
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, - j)
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
data MSegmentTree s a = MSegmentTree (a -> a -> a) (VUM.MVector s a)

-- TODO: Can I UNPACK? the funciton?
-- TODO: Generic queries and immutable segment tree (with `Show` instance)

-- | Creates a new segment tree for `n` leaves.
{-# INLINE newSTree #-}
newSTree :: (VUM.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree (PrimState m) a)
newSTree !f !n !value = MSegmentTree f <$!> VUM.replicate n' value
  where
    !n' = shiftL (bitCeil n) 1

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
{-# INLINE updateLeaf #-}
updateLeaf :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
updateLeaf tree@(MSegmentTree !_ !vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = VUM.length vec `div` 2 - 1
    !i' = i + offset

-- | (Internal) Updates an `MSegmentTree` element (node or leaf) value and their parents up to top root.
{-# INLINE _updateElement #-}
_updateElement :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
_updateElement tree@(MSegmentTree !_ !vec) !i !value = do
  VUM.write vec i value
  _updateParent tree ((i - 1) `div` 2)

-- | (Internal) Recursivelly updates the parent nodes.
{-# INLINE _updateParent #-}
_updateParent :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> m ()
_updateParent _ (-1) = pure () -- REMARK: (-1) `div` 2 == -1
_updateParent _ 0 = pure ()
_updateParent tree@(MSegmentTree !f !vec) !iParent = do
  !c1 <- VUM.read vec (iParent * 2 + 1)
  !c2 <- VUM.read vec (iParent * 2 + 2)
  _updateElement tree iParent (f c1 c2)

-- | Retrieves the folding result over the inclusive range `[l, r]` from `MSegmentTree`.
{-# INLINE querySTree #-}
querySTree :: forall a m. (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> (Int, Int) -> m a
querySTree (MSegmentTree !f !vec) (!lo, !hi) = fromJust <$!> inner 0 (0, initialHi)
  where
    !initialHi = VUM.length vec `div` 2 - 1
    inner :: Int -> (Int, Int) -> m (Maybe a)
    inner !i (!l, !h)
      | lo <= l && h <= hi = Just <$> VUM.read vec i
      | h < lo || hi < l = pure Nothing
      | otherwise = do
        let !d = (h - l) `div` 2
        !ansL <- inner (2 * i + 1) (l, l + d)
        !ansH <- inner (2 * i + 2) (l + d + 1, h)
        pure . Just $ case (ansL, ansH) of
          (Just !a, Just !b) -> f a b
          (Just !a, _) -> a
          (_, Just !b) -> b
          (_, _) -> error "query error (segment tree)"

-- }}}

-- {{{ Dynamic programming

-- let dp = tabulateST f rng (0 :: Int)
--     rng = ((0, 0), (nItems, wLimit))
--     -- type signature can be inferred:
--     f :: forall s. MArray (STUArray s) Int (ST s) => STUArray s (Int, Int) Int -> (Int, Int) -> (ST s) Int
--     f _ (0, _) = return 0
--     f arr (i, w) = do

-- {-# INLINE tabulateST #-}
tabulateST :: forall i. (Ix i) => (forall s. MArray (STUArray s) Int (ST s) => STUArray s i Int -> i -> ST s Int) -> (i, i) -> Int -> UArray i Int
tabulateST !f !bounds_ !e0 =
  runSTUArray uarray
  where
    uarray :: forall s. MArray (STUArray s) Int (ST s) => ST s (STUArray s i Int)
    uarray = do
      !tbl <- newArray bounds_ e0 :: ST s (STUArray s i Int)
      forM_ (range bounds_) $ \ !i -> do
        !e <- f tbl i
        writeArray tbl i e
      return tbl

-- }}}

-- {{{ Graph search

-- TODO: rewrite all

type Graph = Array Int [Int]

-- | Weighted graph (Entry priority payload)
type WGraph = Array Int [IHeapEntry]

-- | Int heap
type IHeap = H.Heap IHeapEntry

-- | Int entry (priority, payload) where priority = cost, payload = vertex
type IHeapEntry = H.Entry Int Int

dfsEveryVertex :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph -> Int -> s -> (s, IS.IntSet)
dfsEveryVertex (!isEnd, !fin, !fout) !graph !start !s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNode (!s, !visits) !x
      | isEnd s = (s, visits)
      | IS.member x visits = (s, visits)
      | otherwise =
        let (!s', !visits') = visitNeighbors (fin s x, IS.insert x visits) x
            -- !_ = traceShow (start, x, graph ! x) ()
         in (fout s' x, visits')

    visitNeighbors :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNeighbors (!s, !visits) !x
      | isEnd s = (s, visits)
      | otherwise = foldl' visitNode (s, visits) (graph ! x)


dfsEveryPath :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph -> Int -> s -> s
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
        foldl' (\ !s2  !n -> visitNode (s2, visits) n) s $ filter (`IS.notMember` visits) (graph ! x)

-- | Searches for a specific route in breadth-first order.
-- | Returns `Just (depth, node)` if succeed.
-- TODO: refactor / test it
bfsFind :: (Int -> Bool) -> Graph -> Int -> Maybe (Int, Int)
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

dijkstra :: forall s. (s -> IHeapEntry -> s) -> s -> WGraph -> Int -> s
dijkstra !f !s0 !graph !start = fst3 $! visitRec (s0, IS.empty, H.singleton $! H.Entry 0 start)
  where
    visitRec :: (s, IS.IntSet, IHeap) -> (s, IS.IntSet, IHeap)
    visitRec (!s, !visits, !heap) =
      case H.uncons heap of
        Just (!x, !heap') ->
          if IS.member (H.payload x) visits
            then visitRec (s, visits, heap')
            else visitRec $ visitNode (s, visits, heap') x
        Nothing -> (s, visits, heap)

    visitNode :: (s, IS.IntSet, IHeap) -> IHeapEntry -> (s, IS.IntSet, IHeap)
    visitNode (!s, !visits, !heap) entry@(H.Entry cost x) =
      let !visits' = IS.insert x visits
          !news = H.fromList . map (first (cost +)) . filter p $ graph ! x
          !p = not . (`IS.member` visits') . H.payload
       in (f s entry, visits', H.union heap news)

-- | Red | Green color
type Color = Bool

-- | Colored vertices in a bipartite graph
type ColorInfo = ([Int], [Int])

-- | DFS with vertices given colors
colorize :: Graph -> IM.IntMap Color -> G.Vertex -> (IM.IntMap Color, Maybe ColorInfo)
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
      | color = Just $ first (v : ) acc
      | otherwise = Just $ second (v : ) acc

-- }}}

-- {{{ Graph search (V2)

-- | BFS template for finding a shortest path from one vertex to another.
bfsPath :: Graph -> Int -> Int -> Maybe Int
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
bfsVerts :: Graph -> Int -> IM.IntMap Int
bfsVerts graph start = inner 0 IM.empty (IS.singleton start)
  where
    inner :: Int -> IM.IntMap Int -> IS.IntSet -> IM.IntMap Int
    inner !depth !vis !vs
      | IS.null vs = vis
      | otherwise = inner (succ depth) vis' vs'
      where
        vis' = IM.union vis $! IM.fromSet (const depth) vs
        vs' = IS.fromList $! filter (`IM.notMember` vis') $! concatMap (graph !) (IS.toList vs)

-- | DFS where all the reachable vertices from one vertex are collcetd
components :: Graph -> Int -> IS.IntSet
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
dj :: WGraph -> Int -> IM.IntMap Int
dj !graph !start = inner (H.singleton $! H.Entry 0 start) IM.empty
  where
    inner !heap !vis
      | H.null heap = vis
      | IM.member v vis = inner heap' vis
      | otherwise = inner heap'' vis'
      where
        (H.Entry cost v, heap') = fromJust $! H.uncons heap
        vis' = IM.insert v cost vis
        vs = map (first (+ cost)) $! filter ((`IM.notMember` vis') . H.payload) $! graph ! v
        heap'' = foldl' (flip H.insert) heap' vs

-- }}}

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
  forM_ [0 .. pred nVerts] $ \ !v ->
    VUM.write dp (ix (v, v)) zeroCost

  -- directly connected vertices
  forM_ edges $ \(!v1, !v2) -> do
    -- let !_ = traceShow (v1, v2, values VU.! v2) ()
    -- (distance, value)
    let !cost = getCost v2
    VUM.write dp (ix (v1, v2)) cost

  return dp
  where
    ix :: (Int, Int) -> Int
    ix = index ((0, 0), (nVerts - 1, nVerts - 1))

{-# INLINE runFW #-}
runFW :: (PrimMonad m, VU.Unbox cost) => (cost -> cost -> cost, cost -> cost -> cost) -> Int -> VUM.MVector (PrimState m) cost -> m ()
runFW (!mergeCost, !minCost) !nVerts !dp = do
  let !ve = pred nVerts
  forM_ (range ((0, 0, 0), (ve, ve, ve))) $ \(!v3, !v1, !v2) -> do
    !cost1 <- VUM.read dp (ix (v1, v2))
    !cost2 <- mergeCost <$> VUM.read dp (ix (v1, v3)) <*> VUM.read dp (ix (v3, v2))
    -- let !_ = traceShow ((v3, v2, v1), cost1, cost2, mergeCost cost1 cost2) ()
    VUM.write dp (ix (v1, v2)) $ minCost cost1 cost2
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

-- | Builds a residual network at initial state.
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

-- ord 'a' == 97
-- ord 'A' == 65
-- indexString = map (subtract 65 . ord)

main :: IO ()
main = do
  [n] <- getLineIntList
  !xs <- getLineIntVec

  let !result = VU.create $ do
        dp <- VUM.replicate n (0 :: Int)

        forM_ [2 .. VU.length result] $ \r -> do
          --   0 1 2 3 r
          -- i = 0 1 2
          --     0   e

          -- all weekdays:
          let e = r - 2
          let !v1 = sum $ map (\i -> xs VU.! min i (e - i)) [0 .. e]

          -- dp:
          !vs <- forM [0 .. r] $ \i -> do
            x1 <- VUM.read dp i
            x2 <- VUM.read dp (r - i)
            return $ x1 + x2

          return $ maximum (v1 : vs)

        return dp

  print "TODO"
