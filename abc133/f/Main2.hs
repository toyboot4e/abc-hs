{-# OPTIONS_GHC -O2 #-}
--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE Strict #-}
--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DerivingStrategies #-}
--{-# LANGUAGE DerivingVia #-}
--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE MagicHash #-}
--{-# LANGUAGE PatternSynonyms #-}
--{-# LANGUAGE UnboxedTuples #-}
--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE NoStarIsType #-}

module Main where

import Debug.Trace
--import Data.Bits
import Data.List
import Data.Char
import Data.Maybe
import Data.Function
--import Data.Ratio
--import Data.Bifunctor
--import Data.Foldable
--import Data.Tuple
--import Data.Complex
--import Numeric
--import Data.Int
--import Data.Proxy
--import Data.Semigroup
import Data.STRef
--import Data.Array.ST
--import qualified Data.Array as A
--import qualified Data.Array.Unboxed as AU
--import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
--import qualified Data.Vector.Algorithms.Intro as VAI
--import qualified Data.Vector.Algorithms.Search as VAS
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
--import qualified Data.Set as S
--import qualified Data.IntSet as IS
--import qualified Data.Map.Strict as M
--import qualified Data.IntMap.Strict as IM
--import qualified Data.Sequence as SQ
--import qualified Data.IntPSQ as IPSQ
--import qualified Data.Heap as H
--import System.IO
--import GHC.Generics
--import GHC.Exts
--import GHC.TypeLits

main :: IO ()
main = do
  [n, q] <- readIntList
  cs <- readIntLists (n - 1)
  xs <- readIntListAll
  putStr . unlines . map show $ solve n q cs xs

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . BS.readInt

readInt :: IO Int
readInt = toInt <$> BS.getLine

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

readIntListAll :: IO [[Int]]
readIntListAll = map toIntList . BS.lines <$> BS.getContents

solve :: Int -> Int -> [[Int]] -> [[Int]] -> [Int]
solve n q cs xs = answer
  where
    answer = step n q cs xs

step :: Int -> Int -> [[Int]] -> [[Int]] -> [Int]
step n q cs xs = runST $ do
  answer <- VUM.replicate q 0
  let graph = buildUndirectedWeightedGraph n $ VU.fromList (map (\[a, b, c, d] -> (a - 1, b - 1, (c, d))) cs)
  let hld = initHLD graph 0
  let xs' = VU.fromList $ map (\[x, y, u, v] -> (x, y, u - 1, v - 1)) xs
  let query = buildCSRDataVec n $ VU.imap (\i (x, y, u, v) -> (u, (i, x, y, 1))) xs' VU.++ VU.imap (\i (x, y, u, v) -> (v, (i, x, y, 1))) xs' VU.++ VU.imap (\i (x, y, u, v) -> (lcaHLD hld u v, (i, x, y, -2))) xs'

  total <- VUM.replicate n 0
  count <- VUM.replicate n 0

  fix (\dfs cur prev dist -> do
    VU.forM_ (query `indexCSR` cur) $ \(i, color, len, weight) -> do
      t <- VUM.unsafeRead total color
      c <- VUM.unsafeRead count color
      VUM.unsafeModify answer (+ ((dist - t + c * len) * weight)) i

    VU.forM_ (getGraphWeightedEdges graph cur) $ \(next, (color, len)) -> do
      when (next /= prev) $ do
        VUM.unsafeModify total (+ len) color
        VUM.unsafeModify count (+ 1) color
        dfs next cur (dist + len)
        VUM.unsafeModify total (subtract len) color
        VUM.unsafeModify count (subtract 1) color) 0 (-1) 0

  VU.toList <$> VU.unsafeFreeze answer

data SparseGraphCSR w = SparseGraphCSR Int (VU.Vector Int) (VU.Vector Int) (VU.Vector (Int, w))

buildUndirectedGraph :: Int -> VU.Vector (Int, Int) -> SparseGraphCSR ()
buildUndirectedGraph n xs = runST $ do
  offset <- VUM.replicate n 0
  VU.forM_ xs $ \(u, v) -> do
    VUM.unsafeModify offset (+ 1) u
    VUM.unsafeModify offset (+ 1) v
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  graph <- VUM.unsafeNew (offset' VU.! n)
  VU.forM_ xs $ \(u, v) -> do
    VUM.unsafeModify offset (subtract 1) u
    o <- VUM.unsafeRead offset u
    VUM.unsafeWrite graph (offset' VU.! u + o) (v, ())
    VUM.unsafeModify offset (subtract 1) v
    o <- VUM.unsafeRead offset v
    VUM.unsafeWrite graph (offset' VU.! v + o) (u, ())
  graph' <- VU.unsafeFreeze graph
  let (edges, _) = VU.unzip graph'
  return $ SparseGraphCSR n offset' edges graph'

buildDirectedGraph :: Int -> VU.Vector (Int, Int) -> SparseGraphCSR ()
buildDirectedGraph n xs = runST $ do
  offset <- VUM.replicate n 0
  VU.forM_ xs $ \(u, _) -> do
    VUM.unsafeModify offset (+ 1) u
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  graph <- VUM.unsafeNew (offset' VU.! n)
  VU.forM_ xs $ \(u, v) -> do
    VUM.unsafeModify offset (subtract 1) u
    o <- VUM.unsafeRead offset u
    VUM.unsafeWrite graph (offset' VU.! u + o) (v, ())
  graph' <- VU.unsafeFreeze graph
  let (edges, _) = VU.unzip graph'
  return $ SparseGraphCSR n offset' edges graph'

buildUndirectedWeightedGraph :: (VU.Unboxable w) => Int -> VU.Vector (Int, Int, w) -> SparseGraphCSR w
buildUndirectedWeightedGraph n xs = runST $ do
  offset <- VUM.replicate n 0
  VU.forM_ xs $ \(u, v, _) -> do
    VUM.unsafeModify offset (+ 1) u
    VUM.unsafeModify offset (+ 1) v
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  graph <- VUM.unsafeNew (offset' VU.! n)
  VU.forM_ xs $ \(u, v, w) -> do
    VUM.unsafeModify offset (subtract 1) u
    o <- VUM.unsafeRead offset u
    VUM.unsafeWrite graph (offset' VU.! u + o) (v, w)
    VUM.unsafeModify offset (subtract 1) v
    o <- VUM.unsafeRead offset v
    VUM.unsafeWrite graph (offset' VU.! v + o) (u, w)
  graph' <- VU.unsafeFreeze graph
  let (edges, _) = VU.unzip graph'
  return $ SparseGraphCSR n offset' edges graph'

buildDirectedWeightedGraph :: (VU.Unboxable w) => Int -> VU.Vector (Int, Int, w) -> SparseGraphCSR w
buildDirectedWeightedGraph n xs = runST $ do
  offset <- VUM.replicate n 0
  VU.forM_ xs $ \(u, _, _) -> do
    VUM.unsafeModify offset (+ 1) u
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  graph <- VUM.unsafeNew (offset' VU.! n)
  VU.forM_ xs $ \(u, v, w) -> do
    VUM.unsafeModify offset (subtract 1) u
    o <- VUM.unsafeRead offset u
    VUM.unsafeWrite graph (offset' VU.! u + o) (v, w)
  graph' <- VU.unsafeFreeze graph
  let (edges, _) = VU.unzip graph'
  return $ SparseGraphCSR n offset' edges graph'

getGraphEdges :: (VU.Unboxable w) => SparseGraphCSR w -> Int -> VU.Vector Int
getGraphEdges (SparseGraphCSR _ offset edges _) v = VU.unsafeSlice (offset VU.! v) (offset VU.! (v + 1) - offset VU.! v) edges

getGraphWeightedEdges :: (VU.Unboxable w) => SparseGraphCSR w -> Int -> VU.Vector (Int, w)
getGraphWeightedEdges (SparseGraphCSR _ offset _ graph) v = VU.unsafeSlice (offset VU.! v) (offset VU.! (v + 1) - offset VU.! v) graph

getGraphVertices :: (VU.Unboxable w) => SparseGraphCSR w -> Int
getGraphVertices (SparseGraphCSR n _ _ _) = n

data HeavyLightDecomposition = HeavyLightDecomposition { rootHLD :: Int,
                                                         parentHLD :: VU.Vector Int,
                                                         heavyHLD :: VU.Vector Int,
                                                         depthHLD :: VU.Vector Int,
                                                         subHLD :: VU.Vector Int,
                                                         vidHLD :: VU.Vector Int,
                                                         inHLD :: VU.Vector Int,
                                                         outHLD :: VU.Vector Int,
                                                         invHLD :: VU.Vector Int,
                                                         headHLD :: VU.Vector Int,
                                                         nodeNum :: Int }

initHLD :: (VU.Unboxable a) => SparseGraphCSR a -> Int -> HeavyLightDecomposition
initHLD graph root = runST $ do
  let n = getGraphVertices graph
  parent <- VUM.replicate n 0
  heavy <- VUM.replicate n 0
  depth <- VUM.replicate n 0
  sub <- VUM.replicate n 0

  let dfs cur prev = do
      VUM.unsafeWrite parent cur prev
      VUM.unsafeWrite sub cur 1
      maxSub <- newSTRef 0
      VUM.unsafeWrite heavy cur (-1)
      VU.forM_ (getGraphEdges graph cur) $ \next -> do
        when (next /= prev) $ do
          v <- VUM.unsafeRead depth cur
          VUM.unsafeWrite depth next (v + 1)
          dfs next cur
          subNext <- VUM.unsafeRead sub next
          VUM.unsafeModify sub (+ subNext) cur
          maxSub' <- readSTRef maxSub
          when (maxSub' < subNext) $ do
            writeSTRef maxSub subNext
            VUM.unsafeWrite heavy cur next

  t <- newSTRef 0
  vid <- VUM.replicate n 0
  inT <- VUM.replicate n 0
  outT <- VUM.replicate n 0
  inv <- VUM.replicate n 0
  headV <- VUM.replicate n 0

  let dfsHLD v = do
      t' <- readSTRef t
      VUM.unsafeWrite vid v t'
      VUM.unsafeWrite inT v t'
      VUM.unsafeWrite inv t' v
      modifySTRef' t (+ 1)
      h' <- VUM.unsafeRead heavy v
      when (0 <= h') $ do
        hv <- VUM.unsafeRead headV v
        VUM.unsafeWrite headV h' hv
        dfsHLD h'
      VU.forM_ (getGraphEdges graph v) $ \next -> do
        dv <- VUM.unsafeRead depth v
        du <- VUM.unsafeRead depth next
        when (dv < du) $ do
          when (h' /= next) $ do
            VUM.unsafeWrite headV next next
            dfsHLD next
      t' <- readSTRef t
      VUM.unsafeWrite outT v t'

  VUM.unsafeWrite parent root root
  dfs root (-1)
  VUM.unsafeWrite headV root root
  dfsHLD root

  parent <- VU.unsafeFreeze parent
  heavy <- VU.unsafeFreeze heavy
  depth <- VU.unsafeFreeze depth
  sub <- VU.unsafeFreeze sub
  vid <- VU.unsafeFreeze vid
  inT <- VU.unsafeFreeze inT
  outT <- VU.unsafeFreeze outT
  inv <- VU.unsafeFreeze inv
  headV <- VU.unsafeFreeze headV

  return $ HeavyLightDecomposition { rootHLD = root, parentHLD = parent, heavyHLD = heavy, depthHLD = depth, subHLD = sub, vidHLD = vid, inHLD = inT, outHLD = outT, invHLD = inv, headHLD = headV, nodeNum = n }

ancestorHLD :: HeavyLightDecomposition -> Int -> Int -> Int
ancestorHLD hld from times
  | times == 1 = parent VU.! from
  | otherwise = inner from times
  where
    root = rootHLD hld
    depth = depthHLD hld
    headV = headHLD hld
    vid = vidHLD hld
    inv = invHLD hld
    parent = parentHLD hld
    inner from times
      | depth VU.! (headV VU.! from) > depth VU.! from - times = if headV VU.! from == root then -1 else inner (parent VU.! (headV VU.! from)) times'
      | otherwise = inv VU.! (vid VU.! from - times)
      where
        times' = times - (depth VU.! from - depth VU.! (headV VU.! from) + 1)

lcaHLD :: HeavyLightDecomposition -> Int -> Int -> Int
lcaHLD hld u v = inner u v
  where
    vid = vidHLD hld
    headV = headHLD hld
    parent = parentHLD hld
    inner u v
      | headV VU.! u == headV VU.! v = if cond then v else u
      | otherwise = if cond then inner (par u) v else inner u (par v)
      where
        cond = vid VU.! u > vid VU.! v
        par x = parent VU.! (headV VU.! x)

distHLD :: HeavyLightDecomposition -> Int -> Int -> Int
distHLD hld u v = depth VU.! u + depth VU.! v - 2 * depth VU.! lcaHLD hld u v
  where
    depth = depthHLD hld

childHLD :: HeavyLightDecomposition -> Int -> Int -> Int -> Int
childHLD hld p c times = ancestorHLD hld c (d - times)
  where
    d = distHLD hld p c

goHLD :: HeavyLightDecomposition -> Int -> Int -> Int -> Int
goHLD hld from to times
  | p == to = ancestorHLD hld from times
  | p == from = childHLD hld from to times
  | dd <= times = goHLD hld p to (times - dd)
  | otherwise = ancestorHLD hld from times
  where
    d = distHLD hld from to
    p = lcaHLD hld from to
    dd = distHLD hld from p

onPathHLD :: HeavyLightDecomposition -> Int -> Int -> Int -> Bool
onPathHLD hld x y a = distHLD hld x a + distHLD hld a y == distHLD hld x y

getPathHLD :: HeavyLightDecomposition -> Int -> Int -> [Int]
getPathHLD hld x y = init xs ++ reverse ys
  where
    p = lcaHLD hld x y
    xs = inner x
    ys = inner y
    inner a
      | a == p = [p]
      | otherwise = a : inner (ancestorHLD hld a 1)

walkPathHLD :: HeavyLightDecomposition -> Int -> Int -> (Int -> Int -> ST s ()) -> ST s ()
walkPathHLD hld x y f = inner1 x p >> inner2 y p
  where
    p = lcaHLD hld x y
    inner1 x p = fix (\loop i -> do
      when (i /= p) $ do
        let v = ancestorHLD hld i 1
        f i v
        loop v) x
    inner2 y p = fix (\loop i -> do
      when (i /= p) $ do
        let v = ancestorHLD hld i 1
        loop v
        f v i) y

foreachNodeQueryHLD :: HeavyLightDecomposition -> Int -> Int -> (Int -> Int -> ST s ()) -> ST s ()
foreachNodeQueryHLD hld x y f = inner x y
  where
    vid = vidHLD hld
    headV = headHLD hld
    parent = parentHLD hld
    inner x y = if vid VU.! x > vid VU.! y then inner y x else f (max (vid VU.! (headV VU.! y)) (vid VU.! x)) (vid VU.! y) >> when (headV VU.! x /= headV VU.! y) (inner x (parent VU.! (headV VU.! y)))

foreachEdgeQueryHLD :: HeavyLightDecomposition -> Int -> Int -> (Int -> Int -> ST s ()) -> ST s ()
foreachEdgeQueryHLD hld x y f = inner x y
  where
    vid = vidHLD hld
    headV = headHLD hld
    parent = parentHLD hld
    inner x y
      | vid VU.! x > vid VU.! y = inner y x
      | headV VU.! x /= headV VU.! y = f (vid VU.! (headV VU.! y)) (vid VU.! y) >> inner x (parent VU.! (headV VU.! y))
      | otherwise = when (x /= y) (f (vid VU.! x + 1) (vid VU.! y))

subTreeNodeQueryHLD :: HeavyLightDecomposition -> Int -> (Int -> Int -> ST s ()) -> ST s ()
subTreeNodeQueryHLD hld i f = f (inHLD hld VU.! i) (outHLD hld VU.! i - 1)

subTreeEdgeQueryHLD :: HeavyLightDecomposition -> Int -> (Int -> Int -> ST s ()) -> ST s ()
subTreeEdgeQueryHLD hld i f = when (inHLD hld VU.! i + 1 <= outHLD hld VU.! i - 1) (f (inHLD hld VU.! i + 1) (outHLD hld VU.! i - 1))

data CSRData v a = CSRData { keySizeCSR :: Int, offsetCSR :: VU.Vector Int, contentsCSR :: v a }

buildCSRData :: (VG.Vector v a, Foldable t) => Int -> t (Int, a) -> CSRData v a
buildCSRData n xs = runST $ do
  offset <- VUM.replicate n 0
  forM_ xs $ \(u, _) -> do
    VUM.unsafeModify offset (+ 1) u
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  VUM.set offset 0
  vs <- VGM.unsafeNew (offset' VU.! n)
  forM_ xs $ \(u, v) -> do
    o <- VUM.unsafeRead offset u
    VGM.unsafeWrite vs (offset' VU.! u + o) v
    VUM.unsafeModify offset (+ 1) u
  vs' <- VG.unsafeFreeze vs
  return $ CSRData n offset' vs'

buildCSRDataVec :: (VG.Vector v a, VG.Vector v (Int, a)) => Int -> v (Int, a) -> CSRData v a
buildCSRDataVec n xs = runST $ do
  offset <- VUM.replicate n 0
  VG.forM_ xs $ \(u, _) -> do
    VUM.unsafeModify offset (+ 1) u
  offset' <- VU.scanl (+) 0 <$> VU.freeze offset
  VUM.set offset 0
  vs <- VGM.unsafeNew (offset' VU.! n)
  VG.forM_ xs $ \(u, v) -> do
    o <- VUM.unsafeRead offset u
    VGM.unsafeWrite vs (offset' VU.! u + o) v
    VUM.unsafeModify offset (+ 1) u
  vs' <- VG.unsafeFreeze vs
  return $ CSRData n offset' vs'

foldCSR' :: (VG.Vector v a) => (b -> v a -> b) -> b -> CSRData v a -> b
foldCSR' f z (CSRData n offset vs) = VU.foldl' (\v (i, j) -> f v (VG.unsafeSlice i (j - i) vs)) z . VU.filter (uncurry (/=)) $ VU.zip offset (VU.tail offset)

ifoldCSR' :: (VG.Vector v a) => (b -> Int -> v a -> b) -> b -> CSRData v a -> b
ifoldCSR' f z (CSRData n offset vs) = VU.foldl' (\v (i, (j, k)) -> f v i (VG.unsafeSlice j (k - j) vs)) z . VU.filter (uncurry (/=) . snd) . VU.indexed $ VU.zip offset (VU.tail offset)

foldMCSR' :: (VG.Vector v a, Monad m) => (b -> v a -> m b) -> b -> CSRData v a -> m b
foldMCSR' f z (CSRData n offset vs) = VU.foldM' (\v (i, j) -> f v (VG.unsafeSlice i (j - i) vs)) z $ VU.zip offset (VU.tail offset)

ifoldMCSR' :: (VG.Vector v a, Monad m) => (b -> Int -> v a -> m b) -> b -> CSRData v a -> m b
ifoldMCSR' f z (CSRData n offset vs) = VU.foldM' (\v (i, (j, k)) -> f v i (VG.unsafeSlice j (k - j) vs)) z . VU.filter (uncurry (/=) . snd) . VU.indexed $ VU.zip offset (VU.tail offset)

forMCSR_ :: (VG.Vector v a, Monad m) => CSRData v a -> (v a -> m b) -> m ()
forMCSR_ (CSRData n offset vs) f = VU.mapM_ (\(i, j) -> f (VG.unsafeSlice i (j - i) vs)) . VU.filter (uncurry (/=)) $ VU.zip offset (VU.tail offset)

iforMCSR_ :: (VG.Vector v a, Monad m) => CSRData v a -> (Int -> v a -> m b) -> m ()
iforMCSR_ (CSRData n offset vs) f = VU.mapM_ (\(i, (j, k)) -> f i (VG.unsafeSlice j (k - j) vs)) . VU.filter (uncurry (/=) . snd) . VU.indexed $ VU.zip offset (VU.tail offset)

ijforMCSR_ :: (VG.Vector v a, Monad m) => CSRData v a -> (Int -> Int -> a -> m b) -> m ()
ijforMCSR_ (CSRData n offset vs) f = VU.mapM_ (\(i, (j, k)) -> VG.imapM_ (\l v -> f i (j + l) v) (VG.unsafeSlice j (k - j) vs)) . VU.filter (uncurry (/=) . snd) . VU.indexed $ VU.zip offset (VU.tail offset)

mapCSR :: (VG.Vector v a, VG.Vector v b) => (a -> b) -> CSRData v a -> CSRData v b
mapCSR f (CSRData n offset vs) = CSRData n offset $ VG.map f vs

mapMCSR :: (VG.Vector v a, VG.Vector v b, Monad m) => (a -> m b) -> CSRData v a -> m (CSRData v b)
mapMCSR f (CSRData n offset vs) = do
  vs' <- VG.mapM f vs
  return $ CSRData n offset vs'

imapCSR :: (VG.Vector v a, VG.Vector v b) => (Int -> v a -> v b) -> CSRData v a -> CSRData v b
imapCSR f (CSRData n offset vs) = CSRData n offset' vs''
  where
    vs' = map (\i -> f i (VG.unsafeSlice (offset VU.! i) (offset VU.! (i + 1) - offset VU.! i) vs)) [0..n - 1]
    offset' = VU.scanl (+) 0 . VU.fromList $ map VG.length vs'
    vs'' = VG.concat vs'

imapMCSR :: (VG.Vector v a, VG.Vector v b, Monad m) => (Int -> v a -> m (v b)) -> CSRData v a -> m (CSRData v b)
imapMCSR f (CSRData n offset vs) = do
  vs' <- mapM (\i -> f i (VG.unsafeSlice (offset VU.! i) (offset VU.! (i + 1) - offset VU.! i) vs)) [0..n - 1]
  let offset' = VU.scanl (+) 0 . VU.fromList $ map VG.length vs'
  let vs'' = VG.concat vs'
  return $ CSRData n offset' vs''

indexCSR :: (VG.Vector v a) => CSRData v a -> Int -> v a
indexCSR (CSRData n offset vs) i = VG.unsafeSlice (offset VU.! i) (offset VU.! (i + 1) - offset VU.! i) vs

contentsSizeCSR :: (VG.Vector v a) => CSRData v a -> Int
contentsSizeCSR (CSRData n offset vs) = VU.last offset
