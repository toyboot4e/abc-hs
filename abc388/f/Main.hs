-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
import Data.Core.SemigroupAction
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

-- combine consecutive intervals into one
combine :: U.Vector (Int, Int) -> U.Vector (Int, Int)
combine = U.fromList . inner . U.toList
  where
    inner :: [(Int, Int)] -> [(Int, Int)]
    inner ((!l1, !r1) : (!l2, !r2) : rest)
      | r1 + 1 == l2 = inner ((l1, r2) : rest)
      | otherwise = (l1, r1) : inner ((l2, r2) : rest)
    inner xs = xs

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m, !a, !b) <- ints4'
  !lrs <- dbgId . (`U.snoc` (n + 1, n + 1)) . (`U.snoc` (n, n)) . combine <$> U.replicateM m ints11'

  let invalidPoints = IS.unions . map (\(!l, !r) -> IS.fromList [l .. r]) $ U.toList lrs

  -- 区間を 2 個飛ばしするケースがまずいか
  -- . . * . * . .

  let !res = note "result" . inner s0 . U.toList $ U.zip lrs (U.tail lrs)
        where
          s0 = IS.singleton 0
          inner :: IS.IntSet -> [((Int, Int), (Int, Int))] -> IS.IntSet
          inner is (((!l, !r), (!nextL, !_)) : rest)
            | IS.null is = is
            | r + 1 - l >= b = IS.empty
            | otherwise =
                let !_ = dbg ("next", is, (l, r), nextL)
                    -- come closer to `l`
                    !leftPoints = note "left 1" . IS.unions $ map (spread (l - 1)) (IS.toList is)
                    -- fill the near left of `l`
                    !leftPoints' = note "left 2" . dropAt (l - 1) $ fill (l - 1) leftPoints
                    -- fill the near right of `r`
                    !rightPoints = note "right" . dropAt (r + 1) . IS.union leftPoints' . IS.fromList $ filter (canJumpTo leftPoints') [r + 1 .. min (r + b) (nextL - 1)]
                 in if nextL == n + 1
                      then leftPoints'
                      else inner rightPoints rest
          spread :: Int -> Int -> IS.IntSet
          spread rmost offset
            | q == 0 = IS.singleton offset
            | otherwise = IS.fromList $ filter (`IS.notMember` invalidPoints) [xl .. xr]
            where
              len = rmost - offset
              q = len `div` b
              -- [a * q .. b * q] are all visitable
              xl = max (rmost - (a + b)) $ offset + a * q
              xr = offset + b * q
          fill :: Int -> IS.IntSet -> IS.IntSet
          fill rmost is0 = foldl' f is0 [(rmost - (a + b)) .. rmost]
            where
              f is i
                | canJumpTo is i = IS.insert i is
                | otherwise = is
          dropAt :: Int -> IS.IntSet -> IS.IntSet
          dropAt rmost = IS.dropWhileAntitone (< rmost - (a + b))
          canJumpTo :: IS.IntSet -> Int -> Bool
          canJumpTo is i = IS.member i is || IS.notMember i invalidPoints && any (`IS.member` is) [i - b .. i - a]

  printYn $ IS.member (n - 1) res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc388/tasks/abc388_f
main :: IO ()
main = runIO solve
