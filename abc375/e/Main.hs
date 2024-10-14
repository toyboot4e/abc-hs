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
-- import Math.BitSet
import Data.SafeList

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- TODO: combine to teams
part3 :: [Int] -> [[(Int, Int)]]
part3 [] = [[]]
part3 (i : is) = do
  t <- [0 .. 2]
  ((t, i) :) <$> part3 is

teams3 :: Int -> [V.Vector (U.Vector Int)]
teams3 n = map f $ part3 [0 .. n - 1]
  where
    f = V.map U.fromList . V.accumulate (flip (:)) (V.replicate 3 []) . V.fromList

solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  -- (team, strong)
  !ixs <- U.replicateM n ((,) <$> int1' <*> int')

  let !xs = U.map snd ixs

  -- -- [(bitA, bitB, bitC)]
  -- let patterns = partitionsOfK (bit n - 1) 3

  let isCompatible :: V.Vector (U.Vector Int) -> Bool
      isCompatible teams = allEq $ V.map toSum teams
        where
          toSum is = U.sum $ U.backpermute xs is
          allEq xs = V.all (== V.head xs) xs

  -- let !_ = dbg teams

  let initialTeams :: V.Vector (S.Set Int)
      initialTeams = V.map S.fromList . V.accumulate (flip (:)) (V.replicate 3 []) $ U.convert $ U.imap (\i (!team, !_) -> (team, i)) ixs

  let countDiff x y = S.size x + S.size y - 2 * S.size (S.intersection x y)
  let eval teams = (`div` 2) . V.sum $ V.zipWith (\ref -> countDiff ref . S.fromList . U.toList) initialTeams teams

  -- [fromList [0,2,4],fromList [1],fromList [3,5]]
  -- (1,      [[5,0],           [4,1],       [3,2]])
  --            2,4,5            4           2,5
  -- let !_ = dbg $ S.difference (S.fromList ([0, 5])) (S.fromList [0, 2, 4])

  let !_ = dbg initialTeams
  let !_ = dbg . map (\t -> (eval t, t)) $ filter isCompatible $ teams3 n

  let candidates = map eval $ filter isCompatible $ teams3 n
  let res = minimumOr (-1) candidates

  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc375/tasks/abc375_e
main :: IO ()
main = runIO solve
