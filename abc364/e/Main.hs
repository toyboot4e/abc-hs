-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree
import Data.Vector.IxVector
import ToyLib.Debug.Grid

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

-- | \(O(N^2 X)\)
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !xMax, !yMax) <- ints3'
  !xys <- U.replicateM n ints2'

  -- (nSel, x) -> minimum y
  let res = U.foldl' step s0 xys
        where
          bnd = force $ zero2 (n + 1) (xMax + 1)
          s0 = IxVector bnd $ U.generate (rangeSize bnd) $ bool maxBound 0 . (== 0)
          step :: IxUVector (Int, Int) Int -> (Int, Int) -> IxUVector (Int, Int) Int
          step sofar (!dx, !dy) = constructIV bnd $ \_ (!nSel, !x) -> case (nSel, x) of
            (0, 0) -> 0
            (0, _) -> maxBound
            _ ->
              let !y0 = sofar @! (nSel, x) :: Int
                  !y' = case sofar @!? (nSel - 1, x - dx) of
                    Nothing -> maxBound
                    Just y_
                      | y_ == maxBound -> maxBound
                      | otherwise -> y_ + dy
               in min y0 y'

  let !_ = dbgGrid res
  let res' = U.maximum . vecIV $ imapIV (\(!nSel, !_) y -> if y <= yMax then nSel else -1) res
  printBSB $ min n (res' + 1)

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc364/tasks/abc364_e
main :: IO ()
main = runIO solve

-- TODO: cheat with pruning
-- solve' :: Int -> Int -> Int -> U.Vector (Int, Int) -> Int
-- solve' n x y xys = do
--   let !res = U.foldl' step s0 xys
--         where
--           !s0 = M.singleton (0, 0) 0
--           step !m (!dx, !dy) = M.unionWith max m m'
--             where
--               m' = M.fromList . mapMaybe f $ M.assocs m
--               f ((!x0, !y0), !cnt)
--                 | x0 > x || y0 > y = Nothing
--                 | otherwise = Just ((x', y'), cnt + 1)
--                 where
--                   !x' = x0 + dx
--                   !y' = y0 + dx
--
--   maximum $ M.elems res
