-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Unsafe.Coerce;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Dsu as Dsu;import qualified AtCoder.Extra.DsuMonoid as DsuM;import qualified AtCoder.Extra.Graph as Gr;import qualified AtCoder.Extra.Vector as EV;import qualified AtCoder.Extra.Vector.Prim as EVP;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.Math as EM;import qualified AtCoder.Extra.HashMap as EHM;import qualified AtCoder.Extra.IntMap as EIM;import qualified AtCoder.Extra.IntSet as EIS;import qualified AtCoder.Extra.IntervalMap as EIT;import AtCoder.Extra.Ix0;import qualified AtCoder.Extra.Monoid.RangeAdd as RangeAdd;import qualified AtCoder.Extra.Monoid.RangeSet as RangeSet;import qualified AtCoder.Extra.Monoid.RollingHash as RH;import qualified AtCoder.Extra.Semigroup.Matrix as Mat;import qualified AtCoder.Extra.Semigroup.Permutation as Permutation;import qualified AtCoder.Extra.Tree as Tr;import qualified AtCoder.Extra.Tree.Hld as Hld;import qualified AtCoder.Extra.Tree.Lct as Lct;import qualified AtCoder.Extra.Tree.TreeMonoid as Tm;import qualified AtCoder.FenwickTree as Ft;import qualified AtCoder.Internal.Assert as ACIA;import qualified AtCoder.Internal.MinHeap as MH;import qualified AtCoder.Internal.Queue as Q;import qualified AtCoder.LazySegTree as LSeg;import qualified AtCoder.ModInt as MI;import qualified AtCoder.SegTree as Seg;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC

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
-- import Data.Vector.CSum

-- }}} toy-lib import
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

op :: Int -> Int -> U.Vector Int -> (Int, U.Vector Int)
op n k hs0 = runST $ do
  -- raise operations only
  -- the highest one has no need of raising
  let hMax = U.maximum hs0

  hs <- U.thaw hs0
  donePush <- UM.replicate n $ Bit False
  heap <- MH.new @_ @(Down Int, Int) (4 * n)

  U.iforM_ hs0 $ \i h -> do
    when (h == hMax) $ do
      MH.push heap (Down h, i)
      GM.write donePush i $ Bit True

  let liftUp h i
        | i < 0 = pure ()
        | i >= n = pure ()
        | otherwise = do
            Bit b <- GM.exchange donePush i $ Bit True
            unless b $ do
              h1 <- GM.read hs i
              MH.push heap (Down h1, i)
              let d = h - h1
              when (d > k) $ do
                modify' (+ (d - k))
                GM.write hs i (h1 + (d - k))

  res <- (`execStateT` (0 :: Int)) $ fix $ \loop -> do
    whenJustM (MH.pop heap) $ \(Down h, !i) -> do
      h' <- GM.read hs i
      when True $ do
        liftUp h' (i - 1)
        liftUp h' (i + 1)
      loop

  (res,) <$> U.unsafeFreeze hs

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !k) <- ints2P
  !hs0 <- intsP

  let res = fst $ U.foldl' (\(!acc, !hs) _ -> first (acc +) $ op n k hs) (0 :: Int, hs0) (U.generate 1000 id)
  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/awc0005/tasks/awc0005_c
main :: IO ()
main = runIO solve
