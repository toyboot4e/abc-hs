-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes
import Data.Buffer

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

try :: Int -> Int -> IxUVector (Int, Int) Int -> U.Vector Int -> Int -> Bool
try h w mat ps initCoin = runST $ do
  let !undef = -1 :: Int
  vec <- IxVector (zero2 h w) <$> UM.replicate (h * w) undef
  mark <- IxVector (zero2 h w) <$> UM.replicate (h * w) False
  writeIV vec (0, 0) initCoin

  buf1 <- newBuffer (h * w)
  buf2 <- newBuffer (h * w)

  pushBack buf1 (0, 0)
  U.iforM_ (U.init ps) $ \i cost -> do
    let buf
          | even i = buf1
          | otherwise = buf2
    let next
          | even i = buf2
          | otherwise = buf1

    fix $ \loop -> do
      whenJustM (popBack buf) $ \(!y, !x) -> do
        cur <- subtract cost . (+ (mat @! (y, x))) <$> readIV vec (y, x)
        when (cur >= 0) $ do
          for_ [(y + 1, x), (y, x + 1)] $ \(!y', !x') -> do
            when (inRange (zero2 h w) (y', x')) $ do
              modifyIV vec (max cur) (y', x')
              b <- exchangeIV mark (y', x') True
              unless b $ do
                -- let !_ = dbg ("push", (y, x), cur, (y', x'))
                pushBack next (y', x')
        loop

  res <- vecIV <$> unsafeFreezeIV vec
  let !_ = dbg (initCoin, G.last res /= undef && G.last res + mat @! (h - 1, w - 1) >= U.last ps, G.last res)
  pure $ G.last res /= undef && G.last res + mat @! (h - 1, w - 1) >= U.last ps

solve :: StateT BS.ByteString IO ()
solve = do
  (!h, !w) <- ints2'
  !mat <- getMat' h w
  ps <- intsU'

  -- Oh dear, bisection method works. It's log, not sqrt. wtf
  let !res = fromJust . bisectR 0 (10 ^ 16 :: Int) $ not . try h w mat ps
  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc415/tasks/abc415_e
main :: IO ()
main = runIO solve
