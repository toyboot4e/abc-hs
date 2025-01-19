-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
import Data.Time.Clock
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

import Data.UnionFind.Mutable
import Data.Graph.Sparse
import Data.Buffer
import ToyLib.Compat
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

{-# INLINE shuffleVector #-}
shuffleVector :: (PrimMonad m, MonadIO m) => UM.MVector (PrimState m) Int -> m ()
shuffleVector vec = do
  gen0 <- initStdGen
  U.foldM'_
    ( \gen u -> do
        let (!v, !gen') = uniformR (0, u) gen
        GM.unsafeSwap vec u v
        pure gen'
    )
    gen0
    (U.generate (GM.length vec) id)

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m, !h) <- ints3'
  !ws <- intsU'
  !uvs <- U.replicateM m ints2'
  !xys <- U.replicateM n ints2'

  let gr = buildSG_ n $ swapDupeU uvs
  resultScore <- UM.replicate 1 (0 :: Int)
  vis0 <- UM.replicate n (Bit False)
  ps0 <- UM.replicate n (-1 :: Int)

  let paintDfs v0 = do
        let dfs d p u = do
              GM.unsafeWrite vis0 u $ Bit True
              GM.unsafeWrite ps0 u p
              GM.unsafeModify resultScore (+ ((d + 1) * (ws G.! u))) 0
              unless (d >= 10) $ do
                U.forM_ (gr `adj` u) $ \v -> do
                  unlessM (unBit <$> GM.unsafeRead vis0 v) $ do
                    dfs (d + 1) u v
        dfs 0 (-1) v0

  let solve vs = do
        GM.set vis0 $ Bit False
        -- GM.set ps0 (-1)
        GM.unsafeWrite resultScore 0 0
        U.forM_ vs $ \v -> do
          unlessM (unBit <$> GM.read vis0 v) $ do
            paintDfs v
        GM.unsafeRead resultScore 0

  let run vs (!score, !ps) = do
        !score' <- solve vs
        if score' > score
          then do
            -- REMARK: do copy!
            ps' <- U.freeze ps0
            pure (score', ps')
          else pure (score, ps)

  start <- liftIO getCurrentTime
  perm <- U.thaw $ U.generate n id

  let runLoop (!score, !ps) = do
        shuffleVector perm
        perm' <- U.unsafeFreeze perm
        (!score', !ps') <- run perm' (score, ps)
        now <- liftIO getCurrentTime
        let !diffSecs = nominalDiffTimeToSeconds $ diffUTCTime now start
        let !diff = diffUTCTime now start
        if diff >= 1.985
          then do
            let !_ = dbg diff
            pure ps'
          else runLoop (score', ps')

  (!s1, !ps1) <- run (U.generate n id) (-1, U.empty)
  (!s2, !ps2) <- run (U.generate n (\i -> n - 1 - i)) (s1, ps1)
  (!s3, !ps3) <- run (U.modify (VAI.sortBy (comparing (ws G.!))) (U.generate n id)) (s2, ps2)
  printVec =<< runLoop (s3, ps3)

-- verification-helper: PROBLEM https://atcoder.jp/contests/ahc041/tasks/ahc041_a
main :: IO ()
main = runIO solve
