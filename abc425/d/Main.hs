-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
import Data.Buffer
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

solve :: StateT BS.ByteString IO ()
solve = do
  (!h, !w) <- ints2'
  -- true for block
  !gr <- mapIV (== '#') <$> getGrid' h w

  let !bnd = zero2 h w
  let isTarget y x = not (gr @! (y, x)) && U.length (U.filter (gr @!) adjs) == 1
        where
          adjs = U.filter (inRange bnd) $ U.map (add2 (y, x)) ortho4

  buf1 <- newBuffer (h * w)
  buf2 <- newBuffer (h * w)
  for_ [0 .. h - 1] $ \y -> do
    for_ [0 .. w - 1] $ \x -> do
      when (isTarget y x) $ do
        pushBack buf1 (y, x)

  -- nBlack <- newMutVar (0 :: Int)
  grVec <- IxVector bnd <$> U.thaw (vecIV gr)
  willVec <- IxVector bnd <$> U.thaw (vecIV gr)
  let isTargetM y x = do
        b1 <- readIV grVec (y, x)
        b2 <- (== 1) . U.length <$> U.filterM (readIV grVec) adjs
        pure $ not b1 && b2
        where
          adjs = U.filter (inRange bnd) $ U.map (add2 (y, x)) ortho4

  flip fix True $ \opLoop is1 -> do
    let b1 = if is1 then buf1 else buf2
    let b2 = if is1 then buf2 else buf1
    clearBuffer b2

    -- mark them all as black
    yxs0 <- unsafeFreezeBuffer b1
    -- modifyMutVar nBlack (+ U.length yxs0)
    U.forM_ yxs0 $ \(!y, !x) -> do
      writeIV grVec (y, x) True
      writeIV willVec (y, x) True

    -- let !_ = dbg yxs0
    -- willVec' <- mapIV (bool '.' '#') <$> unsafeFreezeIV willVec
    -- let !_ = dbgGrid willVec'

    -- pop all and push all adjacent targets
    U.forM_ yxs0 $ \(!y0, !x0) -> do
      let adjs = U.filter (inRange bnd) $ U.map (add2 (y0, x0)) ortho4
      U.forM_ adjs $ \(!y, !x) -> do
        -- not duplicate and is target
        unlessM (readIV willVec (y, x)) $ do
          whenM (isTargetM y x) $ do
            -- push, preventing duplicates
            pushBack b2 (y, x)
            writeIV willVec (y, x) True

    -- process the queue
    unlessM (nullBuffer b2) $ do
      opLoop $ not is1

  printBSB . U.length . U.filter id =<< U.unsafeFreeze (vecIV willVec)

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc425/tasks/abc425_d
main :: IO ()
main = runIO solve
