-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Unsafe.Coerce;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.DsuMonoid as DsuM;import qualified AtCoder.Extra.Graph as Gr;import qualified AtCoder.Extra.Vector as EV;import qualified AtCoder.Extra.Vector.Prim as EVP;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.Math as EM;import qualified AtCoder.Extra.HashMap as EHM;import qualified AtCoder.Extra.IntMap as EIM;import qualified AtCoder.Extra.IntSet as EIS;import qualified AtCoder.Extra.IntervalMap as EIT;import AtCoder.Extra.Ix0;import qualified AtCoder.Extra.Monoid.RangeAdd as RangeAdd;import qualified AtCoder.Extra.Monoid.RangeSet as RangeSet;import qualified AtCoder.Extra.Monoid.RollingHash as RH;import qualified AtCoder.Extra.Semigroup.Matrix as Mat;import qualified AtCoder.Extra.Semigroup.Permutation as Permutation;import qualified AtCoder.Extra.Tree as Tr;import qualified AtCoder.Extra.Tree.Hld as Hld;import qualified AtCoder.Extra.Tree.Lct as Lct;import qualified AtCoder.Extra.Tree.TreeMonoid as Tm;import qualified AtCoder.FenwickTree as Ft;import qualified AtCoder.Internal.MinHeap as MH;import qualified AtCoder.Internal.Queue as Q;import qualified AtCoder.LazySegTree as LSeg;import qualified AtCoder.ModInt as MI;import qualified AtCoder.SegTree as Seg;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC
import AtCoder.Dsu qualified as Dsu
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- 1. Track rects
-- 2. Connect
-- 3. Count

-- swapDeleteFront :: (U.Unbox a) => Q.Queue s a -> Int -> ST s (Maybe a)
-- swapDeleteFront q i = do
--   len <- Q.length q
--   case len of
--     0 -> pure Nothing
--     1 -> Q.popBack q
--     _ | i == len - 1 -> Q.popBack q
--     _ -> do
--       back <- fromJust <$> Q.popBack q
--       cur <- Q.readFront q i
--       Q.writeFront q i back
--       pure cur

isAdj :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
isAdj r1@(!y, !x, !h, !w) r2@(!y', !x', !h', !w')
  | x + w == x' || x' + w' == x =
      let yMin = max y y'
          yMax = min (y + h - 1) (y' + h' - 1)
       in yMin <= yMax
  | y + h == y' || y' + h' == y =
      let xMin = max x x'
          xMax = min (x + w - 1) (x' + w' - 1)
       in xMin <= xMax
  | otherwise = False

-- (2^n)^2 < 10^9. Neraly safe.
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !w0, !h0) <- ints3'
  !cds <- U.replicateM n ((,,) <$> char' <*> int' <*> int')

  -- track rects
  let res1 = U.foldl' step s0 cds
        where
          s0 = U.singleton (0, 0, h0, w0)
          step rects ('X', !atX, !dy) = U.concatMap f rects
            where
              !_ = dbg rects
              f (!y, !x, !h, !w)
                | x + w <= atX = U.singleton (y - dy, x, h, w)
                | x >= atX = U.singleton (y + dy, x, h, w)
                | otherwise = U.fromListN 2 [b1, b2]
                where
                  b1 = (y - dy, x, h, atX - x)
                  b2 = (y + dy, atX, h, x + w - atX)
          step rects ('Y', !atY, !dx) = U.concatMap g rects
            where
              !_ = dbg rects
              g (!y, !x, !h, !w)
                | y + h <= atY = U.singleton (y, x - dx, h, w)
                | y >= atY = U.singleton (y, x + dx, h, w)
                | otherwise = U.fromListN 2 [b1, b2]
                where
                  b1 = (y, x - dx, atY - y, w)
                  b2 = (atY, x + dx, y + h - atY, w)
          step _ _ = error "unreachable"

  -- merge and return counts
  let !_ = dbg res1
  let res = U.filter (/= 0) $ U.create $ do
        let len = G.length res1
        groups <- Dsu.new len
        counts <- U.unsafeThaw $ U.map (\(!_, !_, !h, !w) -> h * w) res1

        U.iforM_ res1 $ \i r -> do
          let rects = U.take i res1
          U.iforM_ rects $ \i' r' -> do
            when (isAdj r r') $ do
              -- change the belonging group
              r1 <- Dsu.leader groups i
              r2 <- Dsu.leader groups i'
              r <- Dsu.merge groups i i'
              -- move the count to the group
              a1 <- GM.exchange counts r1 0
              a2 <- GM.exchange counts r2 0
              GM.write counts r $ a1 + a2
              -- let !_ = dbg ("merge", (i, i'), g, r, r')

        pure counts

  -- area sum check
  let !_ = dbg (U.sum res, h0 * w0)

  printBSB $ U.length res
  printBSB . unwordsBSB $ U.modify VAI.sort res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc432/tasks/abc432_d
main :: IO ()
main = runIO solve
