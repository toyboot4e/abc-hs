-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
import ToyLib.Contest.Bisect
-- import Data.DenseHashMap
import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

import Data.DenseHashMap
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

adj9 :: (Int, Int) -> U.Vector (Int, Int)
adj9 (!y, !x) =
  U.fromListN
    9
    [ (y - 1, x - 1),
      (y - 1, x),
      (y - 1, x + 1),
      (y, x - 1),
      (y, x),
      (y, x + 1),
      (y + 1, x - 1),
      (y + 1, x),
      (y + 1, x + 1)
    ]

dir8 :: Vector (Int, Int)
dir8 =
  U.fromListN
    8
    [ (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1)
    ]

solveSlow :: Int -> Int -> Int -> U.Vector (Int, Int) -> Bool
solveSlow h w k blocks = G.last dist /= -1
  where
    dist = genericBfs grF (h * w) 0 (-1 :: Int)
    !_ = dbg dist
    bnd = zero2 h w
    blocks' = S.fromList $ U.toList blocks
    grF i =
      let (!y, !x) = i `divMod` w
       in G.map (\(!y, !x) -> (index bnd (y, x), 1))
            . G.filter (inRange bnd)
            . G.filter (`S.notMember` blocks')
            $ G.map (add2 (y, x)) ortho4

-- solve :: StateT BS.ByteString IO ()
solve :: Int -> Int -> Int -> U.Vector (Int, Int) -> Bool
solve h w k blocks = runST $ do
  let !dict =
        U.generate h (,w)
          U.++ U.generate h (,-1)
          U.++ U.generate w (-1,)
          U.++ U.generate w (h,)
          U.++ blocks

  let !cap = 9 * G.length blocks + 2 * (h + w)
  hm <- newHM cap
  let !bnd = ((-1, -1), (h + 1, w + 1))

  -- (y, x) -> index
  U.iforM_ dict $ \i (!y, !x) -> do
    -- let !_ = dbg (cap, i, (y, x), bnd)
    let !idx = index bnd (y, x)
    writeHM hm idx i

  uf <- newMUF $ G.length dict
  let unify (!y1, !x1) (!y2, !x2) = do
        i1 <- readMayHM hm $! index bnd (y1, x1)
        i2 <- readMayHM hm $! index bnd (y2, x2)
        case (i1, i2) of
          (Just i1, Just i2) -> unifyMUF_ uf i1 i2
          _ -> pure ()

  -- unify corners:
  for_ [0 .. w - 2] $ \x -> do
    unify (-1, x) (-1, x + 1)
    unify (h, x) (h, x + 1)

  for_ [0 .. h - 2] $ \y -> do
    unify (y, -1) (y + 1, -1)
    unify (y, w) (y + 1, w)

  -- unify blocks
  U.forM_ blocks $ \(!y, !x) -> do
    U.forM_ dir8 $ \(!dy, !dx) -> do
      let !y' = y + dy
      let !x' = x + dx
      let !i' = index bnd (y', x')
      whenM (memberHM hm i') $ do
        unify (y, x) (y', x')

  u <- rootMUF uf =<< readHM hm (index bnd (-1, 0))
  r <- rootMUF uf =<< readHM hm (index bnd (0, w))
  d <- rootMUF uf =<< readHM hm (index bnd (h, 0))
  l <- rootMUF uf =<< readHM hm (index bnd (0, -1))

  b1 <- sameMUF uf u d
  b2 <- sameMUF uf l r
  b3 <- sameMUF uf l u
  b4 <- sameMUF uf d r

  pure . not $ b1 || b2 || b3 || b4

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc413/tasks/abc413_g
main :: IO ()
main = runIO $ do
  (!h, !w, !k) <- ints3'
  when (k == 0) $ do
    printYn True
    liftIO exitSuccess

  !blocks <- U.replicateM k ints11'
  printYn $ solve h w k blocks

propQC :: QC.Gen QC.Property
propQC = do
  h <- QC.choose (1, 4)
  w <- QC.choose (1, 4)
  k <- QC.choose (0, h * w)
  blocks <- (U.uniq . U.modify VAI.sort . U.filter (`notElem` [(0, 0), (h - 1, w - 1)]) . U.fromList <$>) $ QC.vectorOf k do
    y <- QC.chooseInt (0, h - 1)
    x <- QC.chooseInt (0, w - 1)
    pure (y, x)

  pure
    . QC.counterexample (show (h, w, k, blocks))
    $ solve h w k blocks QC.=== solveSlow h w k blocks
  where
    maxN = 1000

runQC :: IO ()
runQC = QC.quickCheck (QC.withMaxSuccess 100 propQC)
