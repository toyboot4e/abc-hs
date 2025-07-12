-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
import ToyLib.Contest.Graph
-- import ToyLib.Contest.Grid
import ToyLib.Contest.Tree

-- import Data.BitSet
import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}



debug :: Bool ; debug = False

{- ORMOLU_ENABLE -}

undef :: Int
undef = maxBound `div` 2

newtype DoNotUnboxStrict a = DoNotUnboxStrict a
  deriving newtype (Show, Eq, Ord)

newtype instance UM.MVector s (DoNotUnboxStrict a) = MV_DoNotUnboxStrict (VM.MVector s a)

newtype instance U.Vector (DoNotUnboxStrict a) = V_DoNotUnboxStrict (V.Vector a)

instance GM.MVector UM.MVector (DoNotUnboxStrict a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ GM.basicLength @V.MVector @a
  basicUnsafeSlice = coerce $ GM.basicUnsafeSlice @V.MVector @a
  basicOverlaps = coerce $ GM.basicOverlaps @V.MVector @a
  basicUnsafeNew = coerce $ GM.basicUnsafeNew @V.MVector @a
  basicInitialize = coerce $ GM.basicInitialize @V.MVector @a
  basicUnsafeReplicate = coerce $ GM.basicUnsafeReplicate @V.MVector @a
  basicUnsafeRead = coerce $ GM.basicUnsafeRead @V.MVector @a
  basicUnsafeWrite = coerce $ GM.basicUnsafeWrite @V.MVector @a
  basicClear = coerce $ GM.basicClear @V.MVector @a
  basicSet = coerce $ GM.basicSet @V.MVector @a
  basicUnsafeCopy = coerce $ GM.basicUnsafeCopy @V.MVector @a
  basicUnsafeMove = coerce $ GM.basicUnsafeMove @V.MVector @a
  basicUnsafeGrow = coerce $ GM.basicUnsafeGrow @V.MVector @a

instance G.Vector U.Vector (DoNotUnboxStrict a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @V.Vector @a
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @V.Vector @a
  basicLength = coerce $ G.basicLength @V.Vector @a
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @V.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @V.Vector @a
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @V.Vector @a
  elemseq _ = seq

instance U.Unbox (DoNotUnboxStrict a)

-- | Add
type OpRepr = DoNotUnboxStrict (U.Vector Int)

instance Semigroup Op where
  -- @new <> old@ on segment tree
  {-# INLINE (<>) #-}
  op1@(Op (DoNotUnboxStrict !x1)) <> op2@(Op (DoNotUnboxStrict !x2))
    | op1 == mempty = op2
    | op2 == mempty = op1
    | G.length x1 <= 2 = Op . DoNotUnboxStrict $ U.zipWith min x1 x2
    | otherwise =
        Op
          . DoNotUnboxStrict
          $ U.zipWith
            min
            (U.generate k $ \i -> if i == k - 1 then x1 G.! i else min (x1 G.! i) (x2 G.! ((k - 2 - i) `mod` k) + k))
            (U.generate k $ \i -> if i == k - 1 then x2 G.! i else min (x2 G.! i) (x1 G.! ((k - 2 - i) `mod` k) + k))
    where
      k = G.length x1

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op $ DoNotUnboxStrict U.empty

instance SemigroupAction Op Acc where
  {-# INLINE sact #-}
  sact op@(Op (DoNotUnboxStrict !vec)) acc@(Acc (!d, DoNotUnboxStrict !x))
    | op == mempty = acc
    | otherwise =
        Acc . (d,) . DoNotUnboxStrict $
          -- toOp と逆向きに回転 (0 -> d')
          let !k = G.length vec
              !d' = (d + 1) `mod` k
              op' = U.map (+ 1) $ G.drop d' vec G.++ G.take (k - d') vec
           in U.zipWith min op' x

-- | Max
type AccRepr = (Int, DoNotUnboxStrict (U.Vector Int))

{- ORMOLU_DISABLE -}
newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !k) <- ints2'
  !es <- U.replicateM (n - 1) ints11'

  let !gr = buildSG n $ swapDupeU es
  let !dist0 = genericBfs (gr `adjW`) n 0 (-1)
  let !_ = dbg dist0

  let res :: U.Vector Acc
      !res = foldTreeAllSG' gr onEdge acc0At toOp
        where
          onEdge :: Op -> (Vertex, Int) -> Op
          onEdge op (!_, 1) = op
          acc0At :: Int -> Acc
          acc0At v =
            let !d = dist0 G.! v
             in Acc . (d `mod` k, ) . DoNotUnboxStrict . U.accumulate min (U.replicate k undef) $ U.singleton (d `mod` k, 0)
          -- 自分が 0 になるように回転する (d -> 0)
          toOp :: Acc -> Op
          toOp (Acc (!d, DoNotUnboxStrict vec)) = Op . DoNotUnboxStrict $ U.take k . U.drop d $ vec U.++ vec

  let repr x
        | x == undef = -1
        | otherwise = x `div` k
  let !_ = dbg res
  printVec $ U.map (\(Acc (!_, DoNotUnboxStrict vec)) -> repr (U.head vec)) $ U.tail res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc414/tasks/abc414_f
main :: IO ()
main = runIO $ do
  t <- int'
  replicateM_ t solve
