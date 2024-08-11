-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
import Data.IntMap

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- FIXME: duplicate code
calc :: Int -> U.Vector Int -> U.Vector Int
calc d xs = dxSumDist
  where
    !n = G.length xs
    !midX = U.head $ U.drop (n `div` 2) xs
    !im = IM.fromListWith (+) $ U.toList $ U.map (, 1 :: Int) xs
    !dxSumDist = dbgId $ U.zipWith (+) toL toR
      where
        atOrigin = U.sum $ U.map (abs . subtract midX) xs
        nl0 = U.length $ U.takeWhile (<= midX) xs
        toL = U.create $ do
          dist <- UM.replicate (d + 1) (0 :: Int)
          U.foldM'
            ( \(!acc, !nl) i -> do
                -- FIXME: double counting at origin
                when (acc <= d && i /= 0) $ do
                  GM.modify dist succ acc
                let !x = midX + i
                    !nr = n - nl
                    !acc' = acc + nl - nr
                    !nl' = nl + fromMaybe 0 (IM.lookup (x + 1) im)
                return (acc', nl')
            )
            (atOrigin, nl0)
            (U.generate (d + 1) id)
          return dist
        nr0 = U.length $ U.dropWhile (< midX) xs
        toR = U.create $ do
          dist <- GM.replicate (d + 1) (0 :: Int)
          U.foldM'
            ( \(!acc, !nr) i -> do
                when (acc <= d) $ do
                  GM.modify dist succ acc
                let !x = midX - i
                    !nl = n - nr
                    !acc' = acc + nr - nl
                    !nr' = nr + fromMaybe 0 (IM.lookup (x - 1) im)
                return (acc', nr')
            )
            (atOrigin, nr0)
            (U.generate (d + 1) id)
          return dist

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !d) <- ints2'
  !xys <- U.replicateM n ints2'

  let !xs = U.modify VAI.sort $ U.map fst xys
  let !xs' = calc d xs
  let !xsSum = csum1D xs'

  let !ys = U.modify VAI.sort $ U.map snd xys
  let !ys' = calc d ys
  let !ysSum = csum1D ys'

  let !res = U.sum . (`U.imap` xs') $ \i nx -> nx * ysSum +! (0, d - i)
  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc366/tasks/abc366_e
main :: IO ()
main = runIO solve
