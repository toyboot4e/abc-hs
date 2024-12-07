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
import Math.Primes
import Math.Divisors

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- testDivisors :: Int -> Int -> Bool
-- testDivisors sq n = inner 0 1
--   where
--     inner !acc i
--       | i == sq = acc == 4
--       | acc > 5 = False
--       | n `rem` i == 0 = inner (acc + 1) (i + 1)
--       | otherwise = inner acc (i + 1)

-- solve :: StateT BS.ByteString IO ()
-- solve = do
--   !n <- int'
--   let res =
--         [ ()
--           | i <- [1 .. isqrt n + 1],
--             i * i <= n,
--             testDivisors i (i * i)
--         ]
--   printBSB $ length res

-- takeP :: Int -> Int -> Int -> [Int] -> [Int]
-- takeP _ 0 !acc _ = pure acc
-- takeP _ _ _ [] = []
-- takeP !target !nRest !acc !ps = do
--   p <- ps
--   let !_ = dbg (acc, p)
--   -- guard $ acc * p ^ nRest <= target
--   guard $ all (\np -> acc * p ^ np <= target) [1 .. nRest]
--   let !ps' = takeWhile (\p' -> acc * p * (p' ^ (nRest - 1)) <= target) $ dropWhile (< p) ps
--   takeP target (nRest - 1) (acc * p) ps'

-- solve :: StateT BS.ByteString IO ()
-- solve = do
--   !n <- int'
--   let ps = takeWhile (\i -> i * i <= n) primes
--   let !res = takeP n 9 1 ps
--   -- let !_ = dbg "uniq?"
--   -- let !res' = U.length . U.uniq . U.modify VAI.sort . U.fromList $ res
--   -- printBSB res'
--   printBSB $ length res

-- solve :: StateT BS.ByteString IO ()
-- solve = do
--   !n <- int'
--   let ps = takeWhile (\i -> i * i <= n) primes
--   let res =
--         [ ()
--           | i <- ps,
--             let ps' = takeWhile (< i) ps,
--             [!p1, !p2] <- combs 2 ps',
--             i * p1 * p2 <= n
--         ]
--   printBSB $ length res

solve :: (HasCallStack) => StateT BS.ByteString IO ()
solve = do
  !n <- int'
  let ps = U.fromList $ takeWhile (\p -> p * p <= n) primes
  let test a b = a * b <= n && a * a <= n && b * b <= n && a * a * b <= n && a * b * b <= n && a * a * b * b <= n
  let res =
        [ U.length i1s
          | let i2s = takeWhile (\i2 -> test 2 (ps G.! i2)) [1 .. U.length ps - 1],
            i2 <- i2s,
            let p2 = ps G.! i2,
            -- let !_ = dbg (i2, p2),
            let i1s = U.takeWhile (`test` p2) $ U.take i2 ps
            -- i1 <- U.toList i1s,
            -- let p1 = ps G.! i1
        ]

  -- let res2 = map (\p -> p * p * p * p * p * p * p * p) $ takeWhile (\p -> p * p * p * p * p * p * p * p <= n) primes
  -- printBSB $ U.length . U.uniq . U.modify VAI.sort . U.fromList $ res2 ++ res
  let res2 = length $ takeWhile (\p -> p * p * p * p * p * p * p * p <= n) primes
  printBSB $ sum res + res2

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc383/tasks/abc383_d
main :: IO ()
main = runIO solve
