-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
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
import Data.Buffer
import Data.SegmentTree.Strict

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !d, !r) <- ints3'
  !ys <- intsU'

  -- The direction is important, and we'll go from the top.
  let !xysSorted = U.modify (VAI.sortBy (comparing (Down . snd))) $ U.indexed ys

  -- Window of values. It only stores points with y s.t. y >= y0 + d
  seg <- newSTree @(Max Int) n

  -- Updates to the segment tree. They're delayed so that the window contains only points with certain y.
  schedulePoints <- newBuffer n
  U.forM_ xysSorted $ pushBack schedulePoints
  schedule <- UM.replicate n mempty

  let recvLoop x0 y0 = do
        -- Run updates than can affect (x0, y0) count calculation:
        let inner = do
              whenJustM (readMaybeFront schedulePoints 0) $ \(!x, !y) -> do
                when (y >= y0 + d) $ do
                  popFront_ schedulePoints
                  writeSTree seg x =<< GM.read schedule x
                  inner

        inner

        -- The update is delayed:
        fromL <- if x0 == 0 then pure mempty else foldSTree seg (max 0 (x0 - r)) (x0 - 1)
        fromR <- if x0 == n - 1 then pure mempty else foldSTree seg (x0 + 1) (min (n - 1) (x0 + r))
        let !_ = dbg (x0, y0, max 0 $ max fromL fromR + Max 1)
        GM.write schedule x0 . max 0 $ max fromL fromR + Max 1

        inner

  U.forM_ xysSorted $ \(!x, !y) -> do
    recvLoop x y

  printBSB . getMax . U.maximum =<< U.unsafeFreeze schedule

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc408/tasks/abc408_f
main :: IO ()
main = runIO solve
