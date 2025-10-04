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

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- @oceajigger
trisect :: (Int, Int) -> (Int -> Int) -> (Int, Int)
trisect (l, r) f
  | r - l <= 2 = (l, r)
  | f m1 > f m2 = trisect (m1, r) f
  | otherwise = trisect (l, m2) f
  where
    m1 = (l * 2 + r) `div` 3
    m2 = (l + r * 2) `div` 3

trisectF64 :: Double -> (Double, Double) -> (Double -> Double) -> (Double, Double)
trisectF64 esp (l, r) f
  | r - l <= esp = (l, r)
  | f m1 > f m2 = trisectF64 esp (m1, r) f
  | otherwise = trisectF64 esp (l, m2) f
  where
    m1 = (l * 2.0 + r) / 3.0
    m2 = (l + r * 2.0) / 3.0

len2 :: Double -> Double -> Double -> Double -> Double
len2 x y x' y' = ((x - x') * (x - x')) + ((y - y') * (y - y'))

len1 :: Double -> Double -> Double -> Double -> Double
len1 x y x' y' = sqrt $ len2 x y x' y'

interpolate :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
interpolate t sx1 sy1 sx2 sy2
  | t * t >= len2 sx1 sy1 sx2 sy2 = (sx2, sy2)
  | otherwise = (sx1 + t * cos theta, sy1 + t * sin theta)
  where
    theta = atan2 (sy2 - sy1) (sx2 - sx1)

solve :: StateT BS.ByteString IO ()
solve = do
  (!sx1, !sy1, !sx2, !sy2) <- ints4'
  (!tx1, !ty1, !tx2, !ty2) <- ints4'

  let l1 = len1 (intToDouble sx1) (intToDouble sy1) (intToDouble sx2) (intToDouble sy2)
  let l2 = len1 (intToDouble tx1) (intToDouble ty1) (intToDouble tx2) (intToDouble ty2)
  let minL = min l1 l2
  let maxL = max l1 l2

  -- eval squared
  let f :: Double -> Double
      f t =
        let (!x1, !y1) = interpolate t (intToDouble sx1) (intToDouble sy1) (intToDouble sx2) (intToDouble sy2)
            (!x2, !y2) = interpolate t (intToDouble tx1) (intToDouble ty1) (intToDouble tx2) (intToDouble ty2)
         in len1 x1 y1 x2 y2

  let eps = 0.00000008 -- 10^-8
  let p (!x, !y) = f ((x + y) / 2.0)
  let res1 = p $ trisectF64 eps (0.0, minL) f
  let res2 = p $ trisectF64 eps (minL, maxL) f

  printBSB $ min res1 res2

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc426/tasks/abc426_e
main :: IO ()
main = runIO $ do
  t <- int'
  replicateM_ t solve
