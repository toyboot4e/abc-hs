-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree
import ToyLib.Debug.Grid
import ToyLib.DP

-- import Data.BitSet
import Data.Vector.IxVector
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
  (!n, !m) <- ints2'
  es <- U.replicateM m ints110'
  q <- int'
  qs <- V.replicateM q $ do
    k <- int'
    U.map pred <$> intsU'

  let !undef = 10 ^ 16 :: Int

  -- use min weight edges
  let !es' =
        U.fromList
          . map U.head
          . U.groupBy ((==) `on` (\(!u, !v, !_) -> (u, v)))
          . U.modify VAI.sort
          $ es
  let !dists = distsNN n undef $ swapDupeW es'

  -- let resolveRoute vs = U.sum $ U.zipWith (\u v -> dists @! (u, v)) vs (U.tail vs)
  let resolveRoute es = fromEdge + fromGr
        where
          fromEdge = U.sum $ U.map thd3 es
          fromGr =
            U.sum $
              U.zipWith
                ( \(!_, !u, !_) (!v, !_, !_) ->
                    -- FIXME:
                    if u == v
                      then 0
                      else dists @! (u, v)
                )
                es
                (U.tail es)

  let solve1 iEdges = U.minimum . U.generate (bit (G.length iEdges)) $ \mask ->
        let !es' =
              U.imap
                ( \i (!u, !v, !w) ->
                    if testBit mask i
                      then (u, v, w)
                      else (v, u, w)
                )
                edges
            -- FIXME:
            !perms = dbgId $ lexPerms (U.generate (G.length iEdges) id)
            -- !perms = V.fromList $ map U.fromList $ permutations [0 .. G.length iEdges - 1]
            !candidates = V.map (resolveRoute . buildRoute es') perms
         in V.minimum candidates
        where
          edges = U.backpermute es iEdges
          buildRoute es is = vs
            where
              !vs = (`U.snoc` (n - 1, n - 1, 0)) . U.cons (0, 0, 0) $ U.backpermute es is

  let res = V.map solve1 qs
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc369/tasks/abc369_e
main :: IO ()
main = runIO solve
