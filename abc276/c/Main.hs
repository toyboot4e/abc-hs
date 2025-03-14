#!/usr/bin/env stack
{- stack script --resolver lts-16.31
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-algorithms --package primitive --package transformers
-}

{- TODOs
- [ ] Try using dfsEveryVertex
- [ ] Better BFS
- [ ] Better dijkstra
- [ ] Easier rolling hash
- [ ] More graph problems
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase, MultiWayIf, PatternGuards, TupleSections #-}
{-# LANGUAGE NumDecimals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies, RankNTypes #-}
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
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
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

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- vector-algorithms: https://www.stackage.org/haddock/lts-16.31/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS

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

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- safe list access
-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead (a : as) = Just a
--
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail (a : as) = Just as

-- sortWith
--
-- sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
-- sortWithDesc = sortBy . flip . comparing
--
-- maximumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- maximumWith = maximumBy . comparing
--
-- minimumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- minimumWith = minimumBy . comparing

-- compress duduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

-- | Returns combinations of the list taking n values.
-- | For example, binary combinations are got by `combination 2 [0..8]`.
-- | REMARK: This is slow. Prefer list comprehension like `x <- [1 .. n], y <- [x + 1 .. n]m ..]`.
combinations :: Int -> [a] -> [[a]]
combinations len elements = comb len (length elements) elements
  where
    comb 0 _ _ = [[]]
    comb r n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
    comb _ _ _ = error "unreachable"

-- }}}

-- {{{ Binary search

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
bsearch (low, high) isOk = bimap wrap wrap (loop (low - 1, high + 1))
  where
    loop :: (Int, Int) -> (Int, Int)
    loop (ok, ng)
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = loop (m, ng)
      | otherwise = loop (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

-- | Monadic variant of `bsearch`
bsearchM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bsearchM (low, high) isOk = bimap wrap wrap <$> loop (low - 1, high + 1)
  where
    loop :: (Int, Int) -> m (Int, Int)
    loop (ok, ng)
      | abs (ok - ng) == 1 = return (ok, ng)
      | otherwise =
        isOk m >>= \yes ->
          if yes
            then loop (m, ng)
            else loop (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

-- }}}

-- {{{ Mutable union-Find tree

-- | Union-find implementation (originally by `@pel`)
newtype MUnionFind s = MUnionFind (VM.MVector s UfNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | `Child parent | Root size`. Not `Unbox` :(
data UfNode = Child {-# UNPACK #-} !Int | Root {-# UNPACK #-} !Int

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF n = MUnionFind <$> VM.replicate n (Root 1)

-- | Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
rootMUF uf@(MUnionFind vec) i = do
  node <- VM.read vec i
  case node of
    Root _ -> return i
    Child p -> do
      r <- rootMUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VM.write vec i (Child r)
      return r

-- | Checks if the two nodes are under the same root.
{-# INLINE sameMUF #-}
sameMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
sameMUF uf x y = liftM2 (==) (rootMUF uf x) (rootMUF uf y)

-- | Just an internal helper.
_unwrapRoot :: UfNode -> Int
_unwrapRoot (Root s) = s
_unwrapRoot (Child _) = undefined

-- | Unites two nodes.
{-# INLINE uniteMUF #-}
uniteMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m ()
uniteMUF uf@(MUnionFind vec) x y = do
  px <- rootMUF uf x
  py <- rootMUF uf y
  when (px /= py) $ do
    sx <- _unwrapRoot <$> VM.read vec px
    sy <- _unwrapRoot <$> VM.read vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (par, chld) = if sx < sy then (px, py) else (py, px)
    VM.write vec chld (Child par)
    VM.write vec par (Root (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind vec) x = do
  px <- rootMUF uf x
  _unwrapRoot <$> VM.read vec px

-- }}}

-- {{{ Sparse union-find tree

-- @gotoki_no_joe
type SparseUnionFind = IM.IntMap Int

newSUF :: SparseUnionFind
newSUF = IM.empty

getRoot :: SparseUnionFind -> Int -> (Int, Int)
getRoot uf i
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, - j)
  | otherwise = getRoot uf j
  where
    j = uf IM.! i

findSUF :: SparseUnionFind -> Int -> Int -> Bool
findSUF uf i j = fst (getRoot uf i) == fst (getRoot uf j)

uniteSUF :: SparseUnionFind -> Int -> Int -> SparseUnionFind
uniteSUF uf i j
  | a == b = uf
  | r >= s = IM.insert a (negate $ r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $ r + s) $ IM.insert a b uf
  where
    (a, r) = getRoot uf i
    (b, s) = getRoot uf j

-- }}}

-- {{{ Digits

-- Taken from <https://hackage.haskell.org/package/digits-0.3.1/docs/Data-Digits.html>

-- digitToInt :: Char -> Int

-- | Returns the digits of a positive integer as a Maybe list, in reverse order or Nothing if a zero
-- | or negative base is given. This is slightly more efficient than in forward order.
mDigitsRev :: Integral n => n -> n -> Maybe [n]
mDigitsRev base i = if base < 1 then Nothing else Just $ dr base i
  where
    dr _ 0 = []
    dr b x = case base of
      1 -> genericTake x $ repeat 1
      _ ->
        let (rest, lastDigit) = quotRem x b
         in lastDigit : dr b rest

-- | Returns the digits of a positive integer as a Maybe list.
--   or Nothing if a zero or negative base is given
mDigits :: Integral n => n -> n -> Maybe [n]
mDigits base i = reverse <$> mDigitsRev base i

-- | Returns the digits of a positive integer as a list, in reverse order.
--   Throws an error if given a zero or negative base.
digitsRev :: Integral n => n -> n -> [n]
digitsRev base = fromJust . mDigitsRev base

-- | Returns the digits of a positive integer as a list.
-- | REMARK: It's modified to return `[0]` when given zero.
digits :: (Eq n, Integral n) => n -> n -> [n]
digits _ 0 = [0]
digits base x = reverse $ digitsRev base x

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n => n -> [n] -> n
unDigits base = foldl' (\a b -> a * base + b) 0

-- | <https://stackoverflow.com/questions/10028213/converting-number-base>
-- | REMARK: It returns `[]` when giben `[0]`. Be sure to convert `[]` to `[0]` if necessary.
convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

-- }}}

-- {{{ Bits

-- TODO: super efficient bit operations

-- | Log base of two or bit floor.
-- | <https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros>
log2 :: (FiniteBits b) => b -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x

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
log2CeilInt x = msb + ceiling
  where
    msb = log2 x
    ceiling = if clearBit x msb > 0 then 1 else 0

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

-- TODO: Generic queries and immutable segment tree (with `Show` instance)

-- | Creates a new segment tree for `n` leaves.
{-# INLINE newTree #-}
newTree :: (VUM.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree (PrimState m) a)
newTree !f !n !value = MSegmentTree f <$> VUM.replicate n' value
  where
    !n' = shiftL (bitCeil n) 1

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
{-# INLINE updateLeaf #-}
updateLeaf :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
updateLeaf tree@(MSegmentTree _ vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = VUM.length vec `div` 2 - 1
    !i' = i + offset

-- | (Internal) Updates an `MSegmentTree` element (node or leaf) value and their parents up to top root.
{-# INLINE _updateElement #-}
_updateElement :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
_updateElement tree@(MSegmentTree _ vec) !i !value = do
  VUM.write vec i value
  _updateParent tree ((i - 1) `div` 2)

-- | (Internal) Recursivelly updates the parent nodes.
{-# INLINE _updateParent #-}
_updateParent :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> m ()
_updateParent _ (-1) = pure () -- REMARK: (-1) `div` 2 == -1
_updateParent _ 0 = pure ()
_updateParent tree@(MSegmentTree f vec) !iParent = do
  !c1 <- VUM.read vec (iParent * 2 + 1)
  !c2 <- VUM.read vec (iParent * 2 + 2)
  _updateElement tree iParent (f c1 c2)

-- | Retrieves the folding result over the inclusive range `[l, r]` from `MSegmentTree`.
{-# INLINE queryByRange #-}
queryByRange :: forall a m. (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> (Int, Int) -> m a
queryByRange (MSegmentTree !f !vec) (!lo, !hi) = fromJust <$> loop 0 (0, initialHi)
  where
    !initialHi = VUM.length vec `div` 2 - 1
    loop :: Int -> (Int, Int) -> m (Maybe a)
    loop !i (!l, !h)
      | lo <= l && h <= hi = Just <$> VUM.read vec i
      | h < lo || hi < l = pure Nothing
      | otherwise = do
        let d = (h - l) `div` 2
        !ansL <- loop (2 * i + 1) (l, l + d)
        !ansH <- loop (2 * i + 2) (l + d + 1, h)
        pure . Just $ case (ansL, ansH) of
          (Just !a, Just !b) -> f a b
          (Just !a, _) -> a
          (_, Just !b) -> b
          (_, _) -> error "query error (segment tree)"

-- }}}

-- {{{ DP

-- Very slow..
type Memo k v = M.Map k v

type Memoized k v = k -> State (Memo k v) v

emptyMemo :: Memo k v
emptyMemo = M.empty

lookupMemo :: Ord k => k -> Memo k v -> Maybe v
lookupMemo = M.lookup

insertMemo :: Ord k => k -> v -> Memo k v -> Memo k v
insertMemo = M.insert

memoize :: Ord k => Memoized k v -> Memoized k v
memoize f k = do
  memo <- gets (lookupMemo k)
  case memo of
    Just v -> return v
    Nothing -> do
      v <- f k
      modify (insertMemo k v)
      return v

evalMemoized :: Memoized a b -> a -> b
evalMemoized s x = evalState (s x) emptyMemo

{-
let dp :: Memoized (Int, Int) Int
    dp = memoize $ \(nRead, nFilled) -> do
      --

let dp' = evalMemoized dp

let result = dp' (n, 7)
-}

-- WARNING: Danger of MLE
tabulateLazy :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulateLazy f bounds_ = array bounds_ [(x, f x) | x <- range bounds_]

{-# INLINE tabulateMap #-}
tabulateMap :: forall i e. (Ix i) => (IM.IntMap e -> i -> e) -> (i, i) -> IM.IntMap e -> IM.IntMap e
tabulateMap f bounds_ cache0 =
  foldl' step cache0 (range bounds_)
  where
    step :: IM.IntMap e -> i -> IM.IntMap e
    step cache i =
      let e = f cache i
       in IM.insert (index bounds_ i) e cache

-- let dp = tabulateST f rng (0 :: Int)
--     rng = ((0, 0), (nItems, wLimit))
--     f :: forall s. MArray (STUArray s) Int (ST s) => STUArray s (Int, Int) Int -> (Int, Int) -> (ST s) Int
--     f _ (0, _) = return 0
--     f arr (i, w) = do
-- {-# INLINE tabulateST #-}
tabulateST :: forall i. (Ix i) => (forall s. MArray (STUArray s) Int (ST s) => STUArray s i Int -> i -> ST s Int) -> (i, i) -> Int -> UArray i Int
tabulateST f bounds_ e0 =
  runSTUArray uarray
  where
    uarray :: forall s. MArray (STUArray s) Int (ST s) => ST s (STUArray s i Int)
    uarray = do
      tbl <- newArray bounds_ e0 :: ST s (STUArray s i Int)
      forM_ (range bounds_) $ \i -> do
        e <- f tbl i
        writeArray tbl i e
      return tbl

-- }}}

-- {{{ ismo 2D

ismo2D :: ((Int, Int), (Int, Int)) -> UArray (Int, Int) Int -> UArray (Int, Int) Int
ismo2D bounds_ seeds = runSTUArray $ do
  arr <- newArray bounds_ (0 :: Int)

  -- row scan
  forM_ (range bounds_) $ \(y, x) -> do
    v <- if x == 0 then return 0 else readArray arr (y, x - 1)
    let diff = seeds ! (y, x)
    writeArray arr (y, x) (v + diff)

  -- column scan
  forM_ (range bounds_) $ \(x, y) -> do
    v <- if y == 0 then return 0 else readArray arr (y - 1, x)
    diff <- readArray arr (y, x)
    writeArray arr (y, x) (v + diff)

  return arr

printMat2D :: (IArray a e, Ix i, Show [e]) => a (i, i) e -> (i, i) -> (i, i) -> IO ()
printMat2D mat ys xs = do
  forM_ (range ys) $ \y -> do
    print $ flip map (range xs) $ \x -> mat ! (y, x)

traceMat2D :: (IArray a e, Ix i, Show e) => a (i, i) e -> (i, i) -> (i, i) -> ()
traceMat2D mat ys xs =
  let !_ = foldl' step () (range ys) in ()
  where
    step _ y = traceShow (map (\(!x) -> mat ! (y, x)) (range xs)) ()

-- }}}

-- {{{ Misc

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVecSorted :: IO (VU.Vector Int)
getLineIntVecSorted = VU.modify VAI.sort <$> getLineIntVec

getLineIntVecSortedDown :: IO (VU.Vector Int)
getLineIntVecSortedDown = VU.modify (VAI.sortBy (comparing Down)) <$> getLineIntVec

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange i j = VU.enumFromN i (j + 1 - i)

-- | From more recent GHC
clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

-- }}}

-- {{{{ Multiset

-- | Multiset: (nKeys, (key -> count))
type MultiSet = (Int, IM.IntMap Int)

emptyMS :: MultiSet
emptyMS = (0, IM.empty)

singletonMS :: Int -> MultiSet
singletonMS x = (1, IM.singleton x 1)

fromListMS :: [Int] -> MultiSet
fromListMS = foldl' (flip incrementMS) emptyMS

incrementMS :: Int -> MultiSet -> MultiSet
incrementMS k (n, im) =
  if IM.member k im
    then (n, IM.insertWith (+) k 1 im)
    else (n + 1, IM.insert k 1 im)

decrementMS :: Int -> MultiSet -> MultiSet
decrementMS k (n, im) =
  case IM.lookup k im of
    Just 1 -> (n - 1, IM.delete k im)
    Just _ -> (n, IM.insertWith (+) k (-1) im)
    Nothing -> (n, im)

-- }}}

-- {{{ Graph

type Graph = Array Int [Int]

dfsEveryVertex :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph -> Int -> s -> (s, IS.IntSet)
dfsEveryVertex (isEnd, fin, fout) graph start s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNode (s, visits) x
      | isEnd s = (s, visits)
      | otherwise =
        let (s', visits') = visitNeighbors (fin s x, IS.insert x visits) x
         in (fout s' x, visits')

    visitNeighbors :: (s, IS.IntSet) -> Int -> (s, IS.IntSet)
    visitNeighbors (s, visits) x
      | isEnd s = (s, visits)
      | otherwise =
        foldl' visitNode (s, visits) $ filter (`IS.notMember` visits) (graph ! x)

dfsEveryPath :: forall s. (s -> Bool, s -> Int -> s, s -> Int -> s) -> Graph -> Int -> s -> s
dfsEveryPath (isEnd, fin, fout) graph start s0 = visitNode (s0, IS.empty) start
  where
    visitNode :: (s, IS.IntSet) -> Int -> s
    visitNode (s, visits) x
      | isEnd s = s
      | otherwise = flip fout x $ visitNeighbors (fin s x, IS.insert x visits) x

    visitNeighbors :: (s, IS.IntSet) -> Int -> s
    visitNeighbors (s, visits) x
      | isEnd s = s
      | otherwise =
        foldl' (\s2 n -> visitNode (s2, visits) n) s $ filter (`IS.notMember` visits) (graph ! x)

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
    bfsRec depth !visits !nbs =
      let -- !_ = traceShow x ()
          !visits' = foldl' (flip IS.insert) visits (IS.toList nbs)
       in let (result, nextNbs) = visitNeighbors nbs
           in case result of
                Just x -> Just (depth, x)
                Nothing -> bfsRec (succ depth) visits' nextNbs
    visitNeighbors :: IS.IntSet -> (Maybe Int, IS.IntSet)
    visitNeighbors nbs =
      let -- !_ = traceShow x ()
          !nbsList = IS.toList nbs
       in foldl'
            ( \(!result, !nbs') !x ->
                let nbs'' = IS.insert x nbs'
                 in if f x
                      then (Just x, nbs'')
                      else (result, nbs'')
            )
            (Nothing, IS.empty)
            nbsList

-- | Weighted graph (Entry priority payload)
type WGraph = Array Int [IHeapEntry]

-- | Int heap
type IHeap = H.Heap IHeapEntry

-- | Int entry
type IHeapEntry = H.Entry Int Int

dijkstra :: forall s. (s -> IHeapEntry -> s) -> s -> WGraph -> Int -> s
dijkstra !f s0 !graph !start =
  let (s, _, _) = visitRec (s0, IS.empty, H.singleton $ H.Entry 0 start)
   in s
  where
    visitRec :: (s, IS.IntSet, IHeap) -> (s, IS.IntSet, IHeap)
    visitRec (!s, !visits, !nbs) =
      case H.uncons nbs of
        Just (x, nbs') ->
          if IS.member (H.payload x) visits
            then visitRec (s, visits, nbs')
            else visitRec $ visitNode (s, visits, nbs') x
        Nothing -> (s, visits, nbs)

    visitNode :: (s, IS.IntSet, IHeap) -> IHeapEntry -> (s, IS.IntSet, IHeap)
    visitNode (!s, !visits, !nbs) entry@(H.Entry cost x) =
      let visits' = IS.insert x visits
          news = H.fromList . map (first (cost +)) . filter p $ graph ! x
          p = not . (`IS.member` visits') . H.payload
       in (f s entry, visits', H.union nbs news)

-- }}}

-- {{{ Integer calculation

-- | Calculates `x * y` but wrapping the result to the maximum boundary.
-- | Works for x >= 0 only.
wrappingMul :: Int -> Int -> Int
wrappingMul x y =
  if (64 - countLeadingZeros x) + (64 - countLeadingZeros y) > 63
    then maxBound @Int
    else x * y

-- | CAUTION: Be aware of the accuracy. Prefer binary search when possible
isqrt :: Int -> Int
isqrt = round @Double . sqrt . fromIntegral

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

-- | Returns `[(prime, count)]`
-- TODO: reuse `primes`
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

-- }}}

-- {{{ Modulo arithmetic

-- TODO: refactor
-- TODO: consider taking `modulus` as the first argument

addMod, subMod, mulMod :: Int -> Int -> Int -> Int
addMod x a modulus = (x + a) `mod` modulus
subMod x s modulus = (x - s) `mod` modulus
mulMod b p modulus = (b * p) `mod` modulus

-- | n! `mod` m
factMod :: Int -> Int -> Int
factMod 0 _ = 1
factMod 1 _ = 1
factMod n m = n * factMod (n - 1) m `rem` m

-- F: Fermet, FC: Fermet by cache

-- | One-shot calculation of $base ^ power `mod` modulo$ in a constant time
powerModConstant :: Int -> Int -> Int -> Int
powerModConstant base power modulo = powerByCache power (powerModCache base modulo)

-- | One-shot calcaulation of $x / d mod p$, using Fermat's little theorem
-- |
-- | 1/d = d^{p-2} (mod p) <=> d^p = d (mod p)
-- |   where the modulus is a prime number and `x` is not a mulitple of `p`
invModF :: Int -> Int -> Int
invModF d modulus = invModFC modulus (powerModCache d modulus)

-- | x / d mod p, using Fermat's little theorem
-- |
-- | 1/d = d^{p-2} (mod p) <=> d^p = d (mod p)
-- |   where the modulus is a prime number and `x` is not a mulitple of `p`
divModF :: Int -> Int -> Int -> Int
divModF x d modulus = x * divModFC x (powerModCache d modulus) `rem` modulus

-- | Cache of base^i for iterative square method
powerModCache :: Int -> Int -> (Int, VU.Vector Int)
powerModCache base modulo = (modulo, VU.fromList $ scanl' (\x _ -> x * x `rem` modulo) base [1 .. 62])

-- | Calculates base^i (mod p) from a cache
powerByCache :: Int -> (Int, VU.Vector Int) -> Int
powerByCache power (modulo, cache) = foldl' step 1 [0 .. 62]
  where
    step acc nBit =
      if testBit power nBit
        then acc * (cache VU.! nBit) `rem` modulo
        else acc

-- | 1/x = x^{p-2} mod p <=> x^p = x mod p
-- |   where the modulus is a prime number
-- |
-- | and x^{p-2} is calculated with cache
invModFC :: Int -> (Int, VU.Vector Int) -> Int
invModFC primeModulus = powerByCache (primeModulus - 2)

divModFC :: Int -> (Int, VU.Vector Int) -> Int
divModFC x context@(modulus, _) = x * invModFC modulus context `rem` modulus

-- }}}

-- ord 'a' == 97
-- ord 'A' == 65
-- indexString = map (subtract 65 . ord)

main :: IO ()
main = do
  [n] <- getLineIntList
  xs <- getLineIntList

  let t = foldl' step s0 $ reverse xs
      s0 = (0, [])

  print t
