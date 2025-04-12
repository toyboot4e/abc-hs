-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
import ToyLib.Contest.Tree

-- import Data.BitSet
import Data.Core.SemigroupAction
import Data.Vector.CSum
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

-- | Max
type OpRepr = Int

instance Semigroup Op where
  -- @new <> old@ on segment tree
  {-# INLINE (<>) #-}
  (Op !x1) <> (Op !x2) = Op $ max x1 x2

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op 0

instance SemigroupAction Op Acc where
  {-# INLINE sact #-}
  sact (Op !len) (Acc !maxLen) = Acc $! max len maxLen

-- | Max
type AccRepr = Int

-- instance Semigroup Acc where
--   {-# INLINE (<>) #-}
--   (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)

-- instance Monoid Acc where
--   {-# INLINE mempty #-}
--   mempty = Acc 0

{- ORMOLU_DISABLE -}
newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}

diameters :: SparseGraph Int -> U.Vector Int
diameters gr = U.map unAcc $ foldTreeAllSG' gr onEdge acc0At toOp
  where
    !n = nVertsSG gr
    onEdge !op (!next, !_dw) = op
    acc0At v = Acc 0
    toOp (Acc len) = Op $ len + 1

solve :: StateT BS.ByteString IO ()
solve = do
  !nA <- int'
  !uvsA <- U.replicateM (nA - 1) ints11'
  let !grA = buildSG nA $ swapDupeU uvsA
  let (!_, !_, !dA) = treeDiameterSG grA (-1)

  !nB <- int'
  !uvsB <- U.replicateM (nB - 1) ints11'
  let !grB = buildSG nB $ swapDupeU uvsB
  let (!_, !_, !dB) = treeDiameterSG grB (-1)

  let !minD = max dA dB
  let diametersA = dbgId $ diameters grA
  let diametersB = dbgId $ diameters grB
  -- let getSum = dbgId . U.sum . U.map (max minD)

  let res = U.sum (U.map f diametersA)
        where
          diaB = U.modify VAI.sort diametersB
          csumDiaB = csum1D diaB
          f :: Int -> Int
          f dA = lenDiscard * minD + lenConnected * (1 + dA) + fromB
            where
              lenDiscard :: Int = fromMaybe nB $ bisectR 0 (nB - 1) $ \i -> dA + diaB G.! i + 1 < minD
              lenConnected :: Int = nB - lenDiscard
              fromB :: Int
                | lenConnected <= 0 = 0
                | otherwise = csumDiaB +! (lenDiscard, nB - 1)

  printBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc401/tasks/abc401_f
main :: IO ()
main = runIO solve
