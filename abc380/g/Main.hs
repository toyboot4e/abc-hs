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
import Data.SegmentTree.Strict
-- import Data.Core.SemigroupAction
import Data.ModInt
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

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

invNum :: (HasCallStack) => Int -> U.Vector Int -> MyModInt
invNum xMax xs = runST $ do
  !stree <- newSTree @(Sum MyModInt) (xMax + 1)
  fmap getSum . (\f -> G.foldM' f mempty xs) $ \acc x -> do
    -- count pre-inserted numbers bigger than this:
    !s <- fromMaybe mempty <$> foldMaySTree stree (x + 1) xMax
    modifySTree stree (+ 1) x
    return $! acc + s

invNumK :: (HasCallStack) => Int -> Int -> U.Vector Int -> U.Vector MyModInt
invNumK xMax k xs = U.create $ do
  let n = U.length xs
  !stree <- newSTree @(Sum MyModInt) (xMax + 1)
  res <- UM.replicate (n - k + 1) $ modInt 0
  (\f -> G.ifoldM' f mempty xs) $ \acc i x -> do
    acc' <-
      if i >= k
        then do
          let iPrev = i - k
          let xPrev = xs G.! iPrev
          delta <- fromMaybe mempty <$> foldMaySTree stree 0 (xPrev - 1)
          -- remove the last
          writeSTree stree xPrev 0
          return $! acc - delta
        else do
          return acc

    -- count pre-inserted numbers bigger than this:
    !s <- fromMaybe mempty <$> foldMaySTree stree (x + 1) xMax
    modifySTree stree (+ 1) x

    -- record the result
    let acc'' = acc' + s
    when (i >= (k - 1)) $ do
      GM.write res (i - k + 1) $ getSum acc''
    return $! acc''
  return res

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !k) <- ints2'
  !xs <- U.map pred <$> intsU'

  -- https://twitter.com/NaoppyJ/status/1732000914259493107
  let dn = modInt k * modInt (k - 1) / modInt 4

  let n0 = invNum (n - 1) xs
  let nTest = n - k + 1
  let res = n0 + dn - U.sum (dbgId (invNumK (n - 1) k xs)) / modInt nTest

  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc380/tasks/abc380_g
main :: IO ()
main = runIO solve
-- main = runIO solveNaive

solveNaive :: StateT BS.ByteString IO ()
solveNaive = do
  (!n, !k) <- ints2'
  !xs <- U.map pred <$> intsU'

  let n0 = invNum (n - 1) xs

  -- https://twitter.com/NaoppyJ/status/1732000914259493107
  let dn = modInt k * modInt (k - 1) / modInt 4
  let nTest = n - k + 1

  -- let f i =
  --       let invNumI = invNum (n - 1) . dbgId $ U.slice i k xs
  --        in n0 - invNumI + dn
  -- let res = sum (dbgId (map f [0 .. n - k])) / modInt nTest

  let f i =
        let invNumI = invNum (n - 1) . dbgId $ U.slice i k xs
         in invNumI
  let res = n0 + dn - sum (dbgId (map f [0 .. n - k])) / modInt nTest

  printBSB res
