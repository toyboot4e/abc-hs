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

-- | Invariant: l < r
type Area = ((Int, Int), (Int, Int))

type Time = Int

none :: (Int, Int)
none = (-1, -1)

-- | +
isCw :: (Int, Int) -> Bool
isCw (!l, !r) = l <= r

-- | Required: both are not none.
intersects :: (Int, Int) -> (Int, Int) -> Bool
intersects lr1@(!l1, !r1) lr2@(!l2, !r2) = l <= r
  where
    r = min r1 r2
    l = max l1 l2

-- | Second parameter is movement. (compatible, fire)
isCompatible :: (Bool, (Int, Int)) -> (Int, Int) -> (Bool, Bool)
isCompatible (!b1, lr1@(!l1, !r1)) (lr2@(!l2, !r2))
  | lr1 == none || lr2 == none = (True, False)
  -- no move
  | l2 == r2 = (not $ intersects lr1 lr2, False)
  | not $ intersects lr1 lr2 = (True, False)
  -- spins
  -- TODO: which should be moved earier
  | isCw lr1 && isCw lr2 && l1 <= l2 = (not b1, l1 <= l2)
  | isCw lr1 && isCw lr2 = (True, l1 <= l2)
  | not (isCw lr1) && not (isCw lr2) && l2 <= l1 = (not b1, l2 <= l1)
  | not (isCw lr1) && not (isCw lr2) = (True, l2 <= l1)
  | otherwise = (False, False)

isCompatibleA :: ((Bool, (Int, Int), (Bool, (Int, Int)))) -> Area -> (Bool, Bool)
isCompatibleA ((!x1, !x2)) ((!y1, !y2)) =
  foldl'
    (\(b1, !b2) (!b3, !b4) -> (b1 && b3, b2 || b4))
    (True, False)
    [isCompatible x1 y1, isCompatible x1 y2, isCompatible x2 y1, isCompatible x2 y2]

-- | +. len = number of elements (my decision was wrong)
areaCW :: Int -> Int -> Int -> Area
areaCW m i len
  | i + len - 1 < m = ((i, i + len - 1), none)
  | otherwise = ((i, m - 1), (0, len - (m - i) - 1))

-- | -. len = number of elements
areaCCW :: Int -> Int -> Int -> Area
areaCCW m i len
  | i - len + 1 >= 0 = ((i, i - len + 1), none)
  | otherwise =
      let !lenRest = len - i - 1
       in ((i, 0), (m - 1, m - lenRest))

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !as <- intsU'
  !bs <- intsU'

  let !res = dbgId $ U.foldl' step s0 $ U.zip as bs
        where
          -- [(cost, area)]
          s0 = [(0 :: Int, (False, (none, none)))]
          step sofar (!a, !b) = do
            move@(!dx, !area2) <- spins
            let candidates = do
                  from@(!acc, !area1) <- sofar
                  guard $ isCompatibleA area1 area2
                  return $! dx + acc
            guard $ not $ null candidates
            let !acc'' = minimum candidates
            return (acc'', area2)
            where
              !_ = dbg sofar
              -- delta
              spins
                | a == b = [(0 :: Int, ((a, a), none)), (m, ((a, m - 1), (0, a))), (m, ((a, 0), (m - 1, a)))]
                | a < b = [(len - 1, areaCW m a len), (lenLoop - 1, areaCCW m a lenLoop)]
                | otherwise = [(lenLoop - 1, areaCW m a lenLoop), (len - 1, areaCCW m a len)]
                where
                  -- be sure to subtarct one for movement costs
                  !len = abs (a - b) + 1
                  !lenLoop = m + 2 - len
                  !_ = dbg ((a, b), (len, lenLoop))

  let !res' = minimum $ map fst res
  printBSB res'

-- verification-helper: PROBLEM https://atcoder.jp/contests/arc182/tasks/arc182_d
main :: IO ()
main = runIO solve
