-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed.Mutable qualified as VUM
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

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

{-# INLINEABLE bipartiteVertexColorsByComponent #-}
bipartiteVertexColorsByComponent :: Int -> (Int -> VU.Vector Int) -> Maybe (V.Vector (VU.Vector Bit))
bipartiteVertexColorsByComponent n gr = runST $ do
  (!isBipartite, !color, !dsu) <- bipartiteVertexColorsImpl n gr
  if isBipartite
    then Just . V.map (VG.backpermute color . VU.filter (< n)) <$> groupsMUF2 dsu
    else pure Nothing

{-# INLINEABLE bipartiteVertexColorsImpl #-}
bipartiteVertexColorsImpl :: Int -> (Int -> VU.Vector Int) -> ST s (Bool, VU.Vector Bit, MUnionFind s)
bipartiteVertexColorsImpl n gr
  | n == 0 = do
      dsu <- newMUF 0
      pure (True, VU.empty, dsu)
  | otherwise = do
      -- 0 <= v < n: red, n <= v: green
      dsu <- newMUF (2 * n)
      for_ [0 .. n - 1] $ \u -> do
        VU.forM_ (gr u) $ \v -> do
          -- try both (red, green) and (green, red) colorings:
          unifyMUF_ dsu (u + n) v
          unifyMUF_ dsu u (v + n)

      color <- VUM.replicate (2 * n) $ Bit False

      -- for each leader vertices, paint their colors:
      for_ [0 .. n - 1] $ \v -> do
        l <- rootMUF dsu v
        when (l == v) $ do
          VGM.write color (v + n) $ Bit True

      -- paint other vertices:
      for_ [0 .. n - 1] $ \v -> do
        VGM.write color v =<< VGM.read color =<< rootMUF dsu v
        VGM.write color (v + n) =<< VGM.read color =<< rootMUF dsu (v + n)

      color' <- VU.unsafeFreeze $ VGM.take n color
      let isCompatible v
            | v >= n = pure True
            | otherwise = do
                c1 <- VGM.read color =<< rootMUF dsu v
                c2 <- VGM.read color =<< rootMUF dsu (v + n)
                if c1 == c2
                  then pure False
                  else isCompatible $ v + 1

      b <- isCompatible 0
      pure (b, color', dsu)

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !es <- U.replicateM m ints11'

  let !gr = buildSG n $ swapDupeU es
  case bipartiteVertexColorsByComponent n (gr `adj`) of
    Nothing -> do
      printBSB "0"
    Just groups -> do
      let nMax = n * (n - 1) `div` 2
      let nIncompatible =
            V.sum $
              V.map
                ( \bs ->
                    let n1 = VU.length $ VU.filter (\(Bit b) -> b) bs
                        n2 = VU.length bs - n1
                     in n1 * (n1 - 1) `div` 2 + n2 * (n2 - 1) `div` 2
                )
                groups
      printBSB $ nMax - nIncompatible - m

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc282/tasks/abc282_d
main :: IO ()
main = runIO solve
