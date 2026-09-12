-- TODO: The top-level comment must be preserved
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.Semigroup;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Unsafe.Coerce;import Data.Ratio;import Data.Bit;import Data.ByteString.Builder qualified as BSB;import Data.ByteString.Char8 qualified as BS;import Data.ByteString.Unsafe qualified as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import Data.Ix.Enum qualified as HT;import Data.List.HT qualified as HT;import Data.Vector.Fusion.Bundle qualified as FB;import Data.Vector.Generic qualified as G;import Data.Vector.Generic.Mutable qualified as GM;import Data.Vector.Primitive qualified as P;import Data.Vector.Unboxed qualified as U;import Data.Vector.Unboxed.Base qualified as U;import Data.Vector.Unboxed.Mutable qualified as UM;import Data.Vector qualified as V;import Data.Vector.Mutable qualified as VM;import Data.Vector.Fusion.Bundle.Monadic qualified as MB;import Data.Vector.Fusion.Bundle.Size qualified as MB;import Data.Vector.Fusion.Stream.Monadic qualified as MS;import Data.Vector.Algorithms.Merge qualified as VAM;import Data.Vector.Algorithms.Intro qualified as VAI;import Data.Vector.Algorithms.Radix qualified as VAR;import Data.Vector.Algorithms.Search qualified as VAS;import Data.IntMap.Strict qualified as IM;import Data.Map.Strict qualified as M;import Data.IntSet qualified as IS;import Data.Set qualified as S;import Data.Sequence qualified as Seq;import Data.Heap qualified as H;import Data.Hashable;import Data.HashMap.Strict qualified as HM;import Data.HashSet qualified as HS;import Test.QuickCheck qualified as QC
import AtCoder.Extra.Bisect qualified as B;import AtCoder.Dsu qualified as Dsu;import AtCoder.Extra.DsuMonoid qualified as DsuM;import AtCoder.Extra.Graph qualified as Gr;import AtCoder.Extra.Vector qualified as EV;import AtCoder.Extra.Vector.Prim qualified as EVP;import AtCoder.Extra.Bisect qualified as B;import AtCoder.Extra.Math qualified as EM;import AtCoder.Extra.HashMap qualified as EHM;import AtCoder.Extra.IntMap qualified as EIM;import AtCoder.Extra.IntSet qualified as EIS;import AtCoder.Extra.IntervalMap qualified as EIT;import AtCoder.Extra.Ix0;import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd;import AtCoder.Extra.Monoid.RangeSet qualified as RangeSet;import AtCoder.Extra.Monoid.RollingHash qualified as RH;import AtCoder.Extra.Semigroup.Matrix qualified as Mat;import AtCoder.Extra.Semigroup.Permutation qualified as Permutation;import AtCoder.Extra.Tree qualified as Tr;import AtCoder.Extra.Tree.Hld qualified as Hld;import AtCoder.Extra.Tree.Lct qualified as Lct;import AtCoder.Extra.Tree.TreeMonoid qualified as Tm;import AtCoder.FenwickTree qualified as Ft;import AtCoder.Internal.Assert qualified as ACIA;import AtCoder.Internal.MinHeap qualified as MH;import AtCoder.Internal.Queue qualified as Q;import AtCoder.LazySegTree qualified as LSeg;import AtCoder.ModInt qualified as MI;import AtCoder.SegTree qualified as Seg;
{- ORMOLU_ENABLE -}

import Algorithm.Bisect (maxRight)
import ToyLib.Contest.Prelude

import Data.SafeList (headMay)
-- import Data.MultiSet2 qualified as MSet
-- import Data.Vector.CSum
-- import Math.BitSet
-- import Math.PowMod

#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif

resolve :: Int -> V.Vector BS.ByteString -> Maybe Int -> BS.ByteString
resolve _ _ Nothing = BS.empty
-- resolve k ss0 (Just i) = mconcat $ s0 : V.toList (V.take (k - 1) ss)
resolve k ss0 (Just i) =
  let -- just sort
      cand1 = BS.dropWhile (== '0') . mconcat . V.toList $ V.modify (VAI.sortBy (comparing Down)) $ V.cons s0 (V.take (k - 1) ss)
      -- preserve the head
      cand2 = BS.dropWhile (== '0') . mconcat . V.toList $ V.cons s0 $ V.modify (VAI.sortBy (comparing Down)) $ V.take (k - 1) ss
  in bool cand2 cand1 $ BS.length cand1 >= BS.length cand2 || cand1 >= cand2
  where
    s0 = ss0 G.! i
    ss = G.ifilter (const . (/= i)) ss0

-- Generated by bundler-hs: https://github.com/toyboot4e/bundler-hs
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !k) <- ints2P

  -- (first non-zero occurence or super big, length, s)
  let feat s =
        let lZero = BS.length $ BS.takeWhile (== '0') s
            l = BS.length s
            t0 = if lZero == l then maxBound `div` 2 else lZero
         in (Down t0, l, s)
  !ss <- V.modify (VAI.sortBy (comparing (Down . feat))) <$> V.replicateM n lineP

  -- TODO: remove dbg
  -- let !_ = dbg ss

  -- Maybe two candidates? Or more?
  let iCand1 = V.findIndex (\s -> BS.head s /= '0') ss
  let iCand2 =
        (fst <$>)
        . headMay
        . V.modify (VAI.sortBy (comparing (feat . snd)))
        . V.map (second (BS.dropWhile (== '0')))
        $ V.indexed ss

  -- let !_ = dbg iCand1
  -- let !_ = dbg iCand2

  let s1 = resolve k ss iCand1
  let s2 = resolve k ss iCand2

  -- let !_ = dbg (s1, feat s1)
  -- let !_ = dbg (s2, feat s2)
  -- let !_ = dbg (feat s1 >= feat s2)

  let s = BS.dropWhile (== '0') $ if feat s1 >= feat s2 then s1 else s2
  if BS.null s
    then printBSB "0"
    else liftIO $ BS.putStrLn s

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc471/tasks/abc471_f
main :: IO ()
main = runIO solve
