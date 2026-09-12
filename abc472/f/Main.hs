-- TODO: The top-level comment must be preserved
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.Semigroup;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Unsafe.Coerce;import Data.Ratio;import Data.Bit;import Data.ByteString.Builder qualified as BSB;import Data.ByteString.Char8 qualified as BS;import Data.ByteString.Unsafe qualified as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import Data.Ix.Enum qualified as HT;import Data.List.HT qualified as HT;import Data.Vector.Fusion.Bundle qualified as FB;import Data.Vector.Generic qualified as G;import Data.Vector.Generic.Mutable qualified as GM;import Data.Vector.Primitive qualified as P;import Data.Vector.Unboxed qualified as U;import Data.Vector.Unboxed.Base qualified as U;import Data.Vector.Unboxed.Mutable qualified as UM;import Data.Vector qualified as V;import Data.Vector.Mutable qualified as VM;import Data.Vector.Fusion.Bundle.Monadic qualified as MB;import Data.Vector.Fusion.Bundle.Size qualified as MB;import Data.Vector.Fusion.Stream.Monadic qualified as MS;import Data.Vector.Algorithms.Merge qualified as VAM;import Data.Vector.Algorithms.Intro qualified as VAI;import Data.Vector.Algorithms.Radix qualified as VAR;import Data.Vector.Algorithms.Search qualified as VAS;import Data.IntMap.Strict qualified as IM;import Data.Map.Strict qualified as M;import Data.IntSet qualified as IS;import Data.Set qualified as S;import Data.Sequence qualified as Seq;import Data.Heap qualified as H;import Data.Hashable;import Data.HashMap.Strict qualified as HM;import Data.HashSet qualified as HS;import Test.QuickCheck qualified as QC
import AtCoder.Extra.Bisect qualified as B;import AtCoder.Dsu qualified as Dsu;import AtCoder.Extra.DsuMonoid qualified as DsuM;import AtCoder.Extra.Graph qualified as Gr;import AtCoder.Extra.Vector qualified as EV;import AtCoder.Extra.Vector.Prim qualified as EVP;import AtCoder.Extra.Bisect qualified as B;import AtCoder.Extra.Math qualified as EM;import AtCoder.Extra.HashMap qualified as EHM;import AtCoder.Extra.IntMap qualified as EIM;import AtCoder.Extra.IntSet qualified as EIS;import AtCoder.Extra.IntervalMap qualified as EIT;import AtCoder.Extra.Ix0;import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd;import AtCoder.Extra.Monoid.RangeSet qualified as RangeSet;import AtCoder.Extra.Monoid.RollingHash qualified as RH;import AtCoder.Extra.Semigroup.Matrix qualified as Mat;import AtCoder.Extra.Semigroup.Permutation qualified as Permutation;import AtCoder.Extra.Tree qualified as Tr;import AtCoder.Extra.Tree.Hld qualified as Hld;import AtCoder.Extra.Tree.Lct qualified as Lct;import AtCoder.Extra.Tree.TreeMonoid qualified as Tm;import AtCoder.FenwickTree qualified as Ft;import AtCoder.Internal.Assert qualified as ACIA;import AtCoder.Internal.MinHeap qualified as MH;import AtCoder.Internal.Queue qualified as Q;import AtCoder.LazySegTree qualified as LSeg;import AtCoder.ModInt qualified as MI;import AtCoder.SegTree qualified as Seg;
{- ORMOLU_ENABLE -}

import ToyLib.Contest.Prelude
import Algorithm.Bisect (maxRight)

-- import Data.MultiSet2 qualified as MSet
import Data.Vector.CSum
-- import Math.BitSet
-- import Math.PowMod

#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif

-- -- TODO: add polarized sort to library
-- -- https://atcoder.jp/contests/abc442/editorial/15136
-- cmpCCW :: (Ord a, Num a) => (a, a) -> (a, a) -> Ordering
-- cmpCCW a b = case compare (half a) (half b) of
--   LT -> GT
--   GT -> LT
--   EQ -> compare 0 (cross a b)
--   where
--     half (x, y) = y > 0 || (y == 0 && x > 0)
-- 
-- cmpCW :: (Ord a, Num a) => (a, a) -> (a, a) -> Ordering
-- cmpCW = flip cmpCCW

-- correct?
-- centroid :: U.Vector (Double, Double) -> (Double, Double)
-- centroid xys =
--   let (!xs, !ys) = U.unzip xys
--       n = intToDouble $ U.length xys
--    in (U.sum xs / n, U.sum ys / n)

-- Generated by bundler-hs: https://github.com/toyboot4e/bundler-hs
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2P
  !xys <- U.replicateM n ints2P
  qs <- U.replicateM q ints11P

  -- Ah... already sorted in CCW
  -- let ixys' = U.modify (VAI.sortBy (comparing (cmpCW . snd))) $ U.indexed xys
  -- let (!_, !xys') = U.unzip ixys'
  -- let iMap = U.update (U.replicate n (-1 :: Int)) $ U.imap (\j (!i, !_) -> (i, j)) ixys'

  let (!xs, !ys) = U.unzip xys
  let csumX = csum1D xs
  let csumY = csum1D ys

  U.forM_ qs $ \(!u, !v) -> do
    let nVerts
           | u <= v = v - u + 1
           | otherwise = (v + n) - u + 1
    let x = intToDouble (csumX +!@ (u, v)) / intToDouble nVerts
    let y = intToDouble (csumY +!@ (u, v)) / intToDouble nVerts
    -- let mx = intToDouble (xs G.! u + xs G.! v) / 2.0
    -- let my = intToDouble (ys G.! u + ys G.! v) / 2.0
    -- let x = (intToDouble (csumX +!@ (u, v)) + mx)/ intToDouble (nVerts + 1)
    -- let y = (intToDouble (csumY +!@ (u, v)) + my)/ intToDouble (nVerts + 1)
    printBSB (x, y)

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc472/tasks/abc472_f
main :: IO ()
main = runIO solve
