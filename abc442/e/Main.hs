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
import Data.Vector.CSum

-- }}} toy-lib import



debug :: Bool ; debug = False

{- ORMOLU_ENABLE -}

shrink :: (Int, Int) -> (Int, Int)
shrink (0, y)
  | y > 0 = (0, 1)
  | otherwise = (0, - 1)
shrink (x, 0)
  | x > 0 =(1, 0)
  | otherwise = (-1, 0)
shrink (!x, !y) = (x `div` g, y `div` g)
  where
    g = abs $ gcd x y

-- quadrant (!x, !y)
--   | x >= 0 && y >= 0 = 1
--   | x <= 0 && y >= 0 = 2
--   | x <= 0 && y <= 0 = 3
--   | otherwise = 4

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2P
  !xys <- U.map shrink <$> U.replicateM n ints2P
  qs <- U.replicateM q ints11P

  -- y/x > y'/x' <-> yx' > y'x
  let cmp (!x, !y) (!x', !y')
        | x == 0 || x' == 0 || y == 0 || y' == 0 = compare (atan2 (intToDouble y) (intToDouble x)) (atan2 (intToDouble y') (intToDouble x'))
        | x > 0 && x' < 0 = LT
        | x' > 0 && x < 0 = GT
        | x > 0 = compare (y * x') (y' * x)
        | x < 0 = compare (-(y * x')) (y' * x)
  let sorted = U.modify (VAI.sortBy cmp) xys
  let !_ = dbg sorted

  let rle = U.fromList . map ((,) <$> U.head <*> U.length) $ U.group sorted
  let !_ = dbg rle
  let toIndex = HM.fromList $ U.toList $ U.imap (\i (!xy, !_) -> (xy, i)) rle

  let len = G.length rle
  let csum = csum1D $ U.map snd rle

  U.forM_ qs $ \(!a, !b) -> do
    let ia = toIndex HM.! (xys G.! a)
    let ib = toIndex HM.! (xys G.! b)
    let cnt
          | ia == ib = snd $ rle G.! ia
          | ia <= ib = csum +! (ia, ib)
          | otherwise = csum +! (ia, len - 1) + csum +! (0, ib)
    let !_ = dbg ((a, b), (ia, ib), cnt)
    printBSB cnt

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc442/tasks/abc442_e
main :: IO ()
main = runIO solve
