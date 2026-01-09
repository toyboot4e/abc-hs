-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Unsafe.Coerce;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Dsu as Dsu;import qualified AtCoder.Extra.DsuMonoid as DsuM;import qualified AtCoder.Extra.Graph as Gr;import qualified AtCoder.Extra.Vector as EV;import qualified AtCoder.Extra.Vector.Prim as EVP;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.Math as EM;import qualified AtCoder.Extra.HashMap as EHM;import qualified AtCoder.Extra.IntMap as EIM;import qualified AtCoder.Extra.IntSet as EIS;import qualified AtCoder.Extra.IntervalMap as EIT;import AtCoder.Extra.Ix0;import qualified AtCoder.Extra.Monoid.RangeAdd as RangeAdd;import qualified AtCoder.Extra.Monoid.RangeSet as RangeSet;import qualified AtCoder.Extra.Monoid.RollingHash as RH;import qualified AtCoder.Extra.Semigroup.Matrix as Mat;import qualified AtCoder.Extra.Semigroup.Permutation as Permutation;import qualified AtCoder.Extra.Tree as Tr;import qualified AtCoder.Extra.Tree.Hld as Hld;import qualified AtCoder.Extra.Tree.Lct as Lct;import qualified AtCoder.Extra.Tree.TreeMonoid as Tm;import qualified AtCoder.FenwickTree as Ft;import qualified AtCoder.Internal.MinHeap as MH;import qualified AtCoder.Internal.Queue as Q;import qualified AtCoder.LazySegTree as LSeg;import qualified AtCoder.ModInt as MI;import qualified AtCoder.SegTree as Seg;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC

-- {{{ toy-lib import
import ToyLib.Contest.Prelude
import Data.Vector.CSum
-- import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes

-- }}} toy-lib import
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

modInt :: Int -> MI.ModInt 998244353
modInt = MI.new


{-# INLINE maxRight #-}
maxRight :: (HasCallStack) => Int -> Int -> (Int -> Bool) -> Int
maxRight l r p = runIdentity $ maxRightM l r (pure . p)

{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
maxRightM l0 r0 = bisectImpl l0 (r0 + 1)

-- | Takes [l, r + 1) or [r, l - 1)
{-# INLINE bisectImpl #-}
bisectImpl :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
bisectImpl l0 r0 p = inner l0 r0
  where
    inner l r
      | abs (r - l) <= 1 = pure l
      | otherwise =
          p mid >>= \case
            True -> inner mid r
            False -> inner l mid
      where
        mid = (l + r) `div` 2

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2P
  !xs <- dbgId . U.modify VAI.sort . U.map modInt <$> intsP
  let !csum = csum1D xs
  !ys <- U.map modInt <$> intsP

  -- Bigger than or equal, less than or equal
  let eval y = lhs + rhs
        where
          nL = maxRight 0 n $ \r -> xs G.! pred r <= y
          nR = n - nL
          !_ = dbg (y, nL)
          lhs
            | nL == 0 = 0
            | otherwise = y * modInt nL - csum +! (0, nL - 1)
          rhs
            | nR == 0 = 0
            | otherwise = csum +! (nL, n - 1) - y * modInt nR

  printBSB . MI.val . U.sum $ U.map eval ys

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc437/tasks/abc437_d
main :: IO ()
main = runIO solve
