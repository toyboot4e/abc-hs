-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

import Data.Graph.Tree.Hld
-- import Data.BitSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes
import Data.UnionFind.Mutable

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

collectMST' :: (Ord w, U.Unbox w) => Int -> U.Vector (Vertex, Vertex, w) -> U.Vector (Vertex, Vertex, w)
collectMST' nVerts sortedEdges = runST $ do
  uf <- newMUF nVerts
  flip U.filterM sortedEdges $ \(!v1, !v2, !_) -> do
    unifyMUF uf v1 v2

solve :: StateT BS.ByteString IO ()
solve = do
  (!h, !w) <- ints2'
  gr <- getMat' h w
  q <- int'
  qs <- U.replicateM q ints6'

    -- answer = u_i - min(hs) + v_i - min(hs)
  --
  -- We're only interested in the minimum height between two vertices and
  -- we can make use of a maximum spanning tree. genious answer:
  -- https://atcoder.jp/contests/abc394/editorial/12276

  let bnd = zero2 h w
  let es =
        U.concatMap
          ( \(!i, !h1) ->
              let (!y, !x) = i `divMod` w
               in if odd (y + x)
                    then U.empty
                    else
                      U.map (\(!y', !x') -> (i, index bnd (y', x'), Min (min h1 (gr @! (y', x')))))
                        . U.filter (inRange bnd)
                        $ U.map (add2 (y, x)) ortho4
          )
          $ U.indexed (vecIV gr)
  let es' = U.modify (VAI.sortBy (comparing (Down . thd3))) es
  let es'' = collectMST' (h * w) es'
  let hld = hldOf $ buildWSG (h * w) $ swapDupeW es''
  tm <- buildEdgeTM hld True $ edgeVertsHLD hld es''

  res <- U.forM qs $ \(dbgId -> (!a, !b, !y, !c, !d, !z)) -> do
    Min h <- foldTM tm (index bnd (a - 1, b - 1)) (index bnd (c - 1, d - 1))
    let !_ = dbg h
    pure $
      if h >= min y z || Min h == mempty
        then abs (y - z)
        else abs (y - h) + abs (z - h)

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc394/tasks/abc394_g
main :: IO ()
main = runIO solve
