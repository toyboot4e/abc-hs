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

import Math.PowMod

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

runtimeAssert :: (HasCallStack) => Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s

{-# INLINE invGcd #-}
invGcd :: Int -> Int -> (Int, Int)
invGcd a0 b
  | a == 0 = (b, 0)
  | otherwise = inner b a 0 1
  where
    !a = a0 `mod` b
    -- Contracts:
    -- [1] s - m0 * a = 0 (mod b)
    -- [2] t - m1 * a = 0 (mod b)
    -- [3] s * |m1| + t * |m0| <= b
    inner :: Int -> Int -> Int -> Int -> (Int, Int)
    inner !s !t !m0 !m1
      | t == 0 =
          let !m' = if m0 < 0 then m0 + b `div` s else m0
           in (s, m')
      | otherwise =
          let !u = s `div` t
              !s' = s - t * u
              !m0' = m0 - m1 * u
           in inner t s' m1 m0'

{-# INLINE invMod #-}
invMod ::
  (HasCallStack) =>
  -- | \(x\)
  Int ->
  -- | \(m\)
  Int ->
  -- | \(x^{-1} \bmod m\)
  Int
invMod x m =
  let !_ = runtimeAssert (1 <= m) $ "AtCoder.Math.invMod: given invalid `m` less than 1: " ++ show m
      (!z1, !z2) = invGcd (fromIntegral x) (fromIntegral m)
      !_ = runtimeAssert (z1 == 1) "AtCoder.Math.invMod: `x^(-1) mod m` cannot be calculated when `gcd x m /= 1`"
   in z2

-- solve :: Int -> U.Vector Int -> U.Vector Int -> StateT BS.ByteString IO ()
-- solve m mods invMods = do
--   !n <- int'
--   !ns <- intsU'
--   let !res = mods G.! U.sum ns * U.foldl' (mulMod m) 1 (U.backpermute invMods ns)
--   let !_ = dbg (mods G.! U.sum ns, U.foldl' (mulMod m) 1 (U.backpermute invMods ns))
--   printBSB res

-- -- verification-helper: PROBLEM https://atcoder.jp/contests/abc425/tasks/abc425_e
-- main :: IO ()
-- main = runIO $ do
--   (!t, !m) <- ints2'
--   let !mods = factModsN m 5001
--   -- let !invMods = U.map (`invMod` m) mods
--   let !invMods = U.map (\x -> let g = gcd x m in invMod (x `div` g) (m `div` g)) mods
--   replicateM_ t $ solve m mods invMods

solve :: Int -> U.Vector Int -> StateT BS.ByteString IO ()
solve m mods = do
  !n <- int'
  !ns <- intsU'
  -- [2, 2] mod 10
  -- 4! = 24      :: 4
  -- 2! * 2! = 4  :: 4
  let !up = mods G.! U.sum ns
  let !down = U.foldl' (mulMod m) 1 (U.backpermute mods ns)
  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc425/tasks/abc425_e
main :: IO ()
main = runIO $ do
  (!t, !m) <- ints2'
  let !mods = factModsN m 5001
  replicateM_ t $ solve m mods
