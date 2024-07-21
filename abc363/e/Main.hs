-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
-- import Data.Buffer
import Data.BinaryHeap
-- import Data.DenseIntSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes
import ToyLib.Debug

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
  (!h, !w, !y) <- ints3'
  !gr <- getMat' h w

  !done <- IxVector (zero2 h w) <$> UM.replicate (h * w) False
  !inserted <- IxVector (zero2 h w) <$> UM.replicate (h * w) False
  let !bnd@(!_, !_) = boundsIV gr

  heap <- newMinBinaryHeap (h * w)

  forM_ [0 .. w - 1] $ \x -> do
    insertBH heap (gr @! (0, x), index bnd (0, x))
    UM.write (vecIV inserted) (index bnd (0, x)) True
    insertBH heap (gr @! (h - 1, x), index bnd (h - 1, x))
    UM.write (vecIV inserted) (index bnd (h - 1, x)) True

  forM_ [1 .. h - 2] $ \y -> do
    insertBH heap (gr @! (y, 0), index bnd (y, 0))
    UM.write (vecIV inserted) (index bnd (y, 0)) True
    insertBH heap (gr @! (y, w - 1), index bnd (y, w - 1))
    UM.write (vecIV inserted) (index bnd (y, w - 1)) True

  rest <- UM.replicate 1 (h * w)

  res <- U.generateM y $ \((+ 1) -> gen) -> do
    let !_ = dbg gen
    fix $ \loop -> do
      whenJustM (viewBH heap) $ \(!y0, !i) -> do
        let !_ = dbgId (gen, i)
        let !y' = y0 - gen
        when (y' <= 0) $ do
          GM.write (vecIV done) i True
          unsafeDeleteBH_ heap
          GM.modify rest (subtract 1) 0
          U.forM_ (ortho4' bnd (i `divMod` w)) $ \(!y', !x') -> do
            let !i' = index bnd (y', x')
            unlessM (UM.read (vecIV inserted) i') $ do
              insertBH heap (vecIV gr G.! i', i')
              UM.write (vecIV inserted) i' True
          loop

    UM.read rest 0

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc363/tasks/abc363_e
main :: IO ()
main = runIO solve
