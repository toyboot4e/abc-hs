-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
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
import Data.IntervalMap

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

calc :: [(Int, Int)] -> [(Int, Int)] -> Int
calc [] _ = 0
calc _ [] = 0
calc xxs@((!xl, !xr) : xs) yys@((!yl, !yr) : ys)
  | xr < yl = calc xs yys
  | yr < xl = calc xxs ys
  | xl >= yl && xr <= yr = (xr - xl + 1) + calc xs yys
  | yl >= xl && yr <= xr = (yr - yl + 1) + calc xxs ys
  | xr < yr =
      let !l = max xl yl
          !r = min xr yr
       in (r - l + 1) + calc xs yys
  | otherwise =
      let !l = max xl yl
          !r = min xr yr
       in (r - l + 1) + calc xxs ys

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  tps <- U.replicateM m ((,) <$> int' <*> int1')
  !q <- int'
  qs <- U.replicateM q ints11'

  let tpsByPerson_ = V.map swap . U.convert $ U.modify (VAI.sortBy (comparing fst)) tps :: V.Vector (Int, Int)
  let !tpsByPerson = V.map U.fromList . V.accumulate (flip (:)) (V.replicate n ([] :: [Int])) $ tpsByPerson_ :: V.Vector (U.Vector Int)

  let !intervals = (`V.imap` tpsByPerson) $ \iPerson ts ->
        let !nIntervals = (U.length ts + 1) `div` 2
         in U.foldl'
              ( \set i ->
                  let !t1 = ts U.! (2 * i + 0) - 1 -- REMARK: reverse making it complicated
                      !t2 = ts U.! (2 * i + 1)
                   in -- in ((t1, t2) : set)
                      ((t2, t1) : set)
              )
              []
              (U.generate nIntervals id)

  let !res = U.map (\(!i1, !i2) -> calc (intervals G.! i1) (intervals G.! i2)) qs
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc365/tasks/abc365_f
main :: IO ()
main = runIO solve
