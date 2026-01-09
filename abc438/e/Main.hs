-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Unsafe.Coerce;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Dsu as Dsu;import qualified AtCoder.Extra.DsuMonoid as DsuM;import qualified AtCoder.Extra.Graph as Gr;import qualified AtCoder.Extra.Vector as EV;import qualified AtCoder.Extra.Vector.Prim as EVP;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.Math as EM;import qualified AtCoder.Extra.HashMap as EHM;import qualified AtCoder.Extra.IntMap as EIM;import qualified AtCoder.Extra.IntSet as EIS;import qualified AtCoder.Extra.IntervalMap as EIT;import AtCoder.Extra.Ix0;import qualified AtCoder.Extra.Monoid.RangeAdd as RangeAdd;import qualified AtCoder.Extra.Monoid.RangeSet as RangeSet;import qualified AtCoder.Extra.Monoid.RollingHash as RH;import qualified AtCoder.Extra.Semigroup.Matrix as Mat;import qualified AtCoder.Extra.Semigroup.Permutation as Permutation;import qualified AtCoder.Extra.Tree as Tr;import qualified AtCoder.Extra.Tree.Hld as Hld;import qualified AtCoder.Extra.Tree.Lct as Lct;import qualified AtCoder.Extra.Tree.TreeMonoid as Tm;import qualified AtCoder.FenwickTree as Ft;import qualified AtCoder.Internal.MinHeap as MH;import qualified AtCoder.Internal.Queue as Q;import qualified AtCoder.LazySegTree as LSeg;import qualified AtCoder.ModInt as MI;import qualified AtCoder.SegTree as Seg;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC

import AtCoder.Internal.Assert qualified as ACIA
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
import Data.Vector.CSum

-- }}} toy-lib import
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

newtype PermutationWithSemigroup a = PermutationWithSemigroup
  { unPermutationWithSemigroup :: U.Vector (Int, a)
  }
  deriving newtype
    (Eq, Show)

{-# INLINE new #-}
new :: (HasCallStack, Semigroup a, U.Unbox a) => U.Vector (Int, a) -> PermutationWithSemigroup a
new ixs = PermutationWithSemigroup ixs
  where
    n = U.length ixs
    !_ = U.foldl' (\() (!i, !_) -> let !_ = ACIA.runtimeAssert (-1 <= i && i < n) "AtCoder.Extra.Semigroup.Permutation.new: index boundary error" in ()) () ixs

-- | @since 1.1.0.0
instance (Semigroup a, U.Unbox a) => Semigroup (PermutationWithSemigroup a) where
  {-# INLINE (<>) #-}
  PermutationWithSemigroup p2 <> PermutationWithSemigroup p1 = PermutationWithSemigroup $ U.map f p1
    where
      !_ = ACIA.runtimeAssert (U.length p2 == U.length p1) "AtCoder.Extra.Semigroup.PermutationWithSemigroup.(<>): length mismatch"
      f (-1, !a) = (-1, a)
      f (i, !a) =
        let (!i', !aNew) = G.unsafeIndex p2 i
         in (i', aNew <> a)

{-# INLINE act #-}
act :: (HasCallStack, Semigroup a, U.Unbox a) => PermutationWithSemigroup a -> (Int, a) -> (Int, a)
act (PermutationWithSemigroup vec) (!i, !a) = case vec G.! i of
  (-1, !_) -> (i, a)
  (i', !aOld) -> (i', a <> aOld)

-- | Binary lifting
cacheBL :: (Semigroup a) => a -> V.Vector a
cacheBL s = V.constructN 30 $ \sofar ->
  if G.null sofar
    then s
    else
      let !s1 = G.last sofar
       in s1 <> s1

-- | Binary lifting
{-# INLINE sactBL #-}
sactBL :: (HasCallStack, Semigroup s, G.Vector v s) => v s -> (s -> a -> a) -> Int -> a -> a
sactBL cache sactF n a0
  | n == 0 = a0
  | otherwise = G.ifoldl' f a0 cache
  where
    f !a iBit !s
      | testBit n iBit = sactF s a
      | otherwise = a

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2P
  !xs <- U.map pred <$> intsP
  !qs <- U.replicateM q ((,) <$> intP <*> int1P)

  let perm = new $ U.imap (\i i' -> (i', Sum (i + 1))) xs
  let cache = cacheBL perm
  let res = U.map (\(!nOp, !i0) -> sactBL cache act nOp (i0, Sum (0 :: Int))) qs

  printBSB $ unlinesBSB $ U.map (getSum . snd) res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc438/tasks/abc438_e
main :: IO ()
main = runIO solve
