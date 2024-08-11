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
import Data.SegmentTree.Strict
import Data.ModInt
import Math.Stimes

-- import Data.Graph.TwoSat
-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

pattern WIP, L, R, INCOMPATIBLE :: Int
pattern WIP = 0
pattern L = 1
pattern R = 2
pattern INCOMPATIBLE = -1

assign :: (HasCallStack, PrimMonad m) => UM.MVector (PrimState m) Int -> Int -> Int -> m ()
assign vec i state = do
  x <- GM.read vec i
  case (x, state) of
    (WIP, _) -> UM.write vec i state
    (L, R) -> UM.write vec i INCOMPATIBLE
    (R, L) -> UM.write vec i INCOMPATIBLE
    _ -> return ()

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  !pvs <- U.replicateM q (first pred <$> ints2')

  wip <- UM.replicate q WIP

  do -- 自分よりも大きいものが過去に左右にいたか
    stree <- buildSTree $ U.replicate n (mempty @(Max Int))
    U.iforM_ pvs $ \iq (!i, !v) -> do
      Max maxL <- foldSTree stree 0 i
      Max maxR <- foldSTree stree i (n - 1)
      when (maxL > v) $ do
        assign wip iq R
      when (maxR > v) $ do
        assign wip iq L
      modifySTree stree (<> Max v) i

  do -- 自分よりも小さいものが将来左右にいるか
    stree <- buildSTree $ U.replicate n (mempty @(Min Int))
    U.iforM_ (U.reverse pvs) $ \iq_ (!i, !v) -> do
      let !iq = q - 1 - iq_
      Min minL <- foldSTree stree 0 i
      Min minR <- foldSTree stree i (n - 1)
      when (minL < v) $ do
        assign wip iq R
      when (minR < v) $ do
        assign wip iq L
      modifySTree stree (<> Min v) i

  assignments <- U.unsafeFreeze wip

  when (U.any (== INCOMPATIBLE) assignments) $ do
    printBSB '0'
    liftIO exitSuccess

  let !nWip = G.length $ G.filter (== WIP) assignments
  let !res = if nWip == 0 then modInt 1 else mulTimes nWip (*) (modInt 2)

  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/arc182/tasks/arc182_a
main :: IO ()
main = runIO solve
