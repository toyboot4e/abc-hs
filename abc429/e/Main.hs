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

-- | Add
type OpRepr = Int

instance Semigroup Op where
  -- @new <> old@ on segment tree
  {-# INLINE (<>) #-}
  (Op !x1) <> (Op !x2) = Op (x1 + x2)

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op 0

-- instance SemigroupAction Op Acc where
--   {-# INLINE sact #-}
--   sact (Op !dx) (Acc !x) = Acc (x + dx)

-- instance SegmentAction Op Acc where
--   {-# INLINE segActWithLength #-}
--   segActWithLength len (Op !dx) (Acc !x) = Acc (x + dx)

-- | Min2 ((d1, v1), (d2, v2))
type AccRepr = ((Int, Int), (Int, Int))

none :: (Int, Int)
none = (maxBound, -1)

instance Semigroup Acc where
  {-# INLINE (<>) #-}
  a@(Acc (k1@(!d1, !v1), k2@(!d2, !v2))) <> a'@(Acc (k1'@(!d1', !v1'), k2'@(!d2', !v2')))
    | k1 == none = a'
    | k1' == none = a
    | otherwise =
      -- TODO: faster
      -- FIXME: nub by verts, not by dists
      let [!a, !b] = take 2 . nubBy ((==) `on` snd) $ sort [k1, k2, k1', k2']
       in Acc (a, b)

onAct :: Acc -> Acc
onAct a@(Acc (k1@(!d1, !v1), k2@(!d2, !v2)))
  | k1 == none = a
  | k2 == none = Acc ((d1 + 1, v1), none)
  | otherwise = Acc ((d1 + 1, v1), (d2 + 1, v2))

instance Monoid Acc where
  {-# INLINE mempty #-}
  mempty = Acc (none, none)

{- ORMOLU_DISABLE -}
newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}

{-# INLINE adj1 #-}
adj1 :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (Vertex, Int)
adj1 SparseGraph {..} v = U.zip vs (U.replicate nVertsSG (1 :: Int))
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG
    !ws = U.unsafeSlice o1 (o2 - o1) edgeWeightsSG

-- Min2 monoid?
-- - BFS tree
--
-- BFS + DP
-- - starting from low degree vertex
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !uvs <- U.replicateM m ints11'
  !safe <- U.map (== 'S') . U.fromList . BS.unpack <$> line'

  let !gr = buildSG_ n $ swapDupeU uvs
  ms <- U.unsafeThaw $ U.imap (\v s -> if s then Acc ((0, v), none) else mempty) safe

  -- O(n + m)
  let (!dist, !prev) = genericBfsTree (gr `adj1`) n 0 (-1 :: Int)
  -- let !_ = dbg prev
  -- let !_ = dbg next

  -- O(n \log n) run actions from leaves to the root
  let !farVerts = U.modify (VAI.sortBy (comparing (Down . (dist G.!)))) (U.generate n id)
  U.forM_ farVerts $ \v -> do
    let !p = prev G.! v
    when (p /= -1) $ do
      mp <- GM.read ms p
      let !_ = dbg ("act", p, v)
      GM.modify ms (onAct mp <>) v

  -- O(n (\log n + m)) run actions from the root to the leaves
  let !nearVerts = U.modify (VAI.sortBy (comparing (dist G.!))) (U.generate n id)
  U.forM_ nearVerts $ \u -> do
    U.forM_ (gr `adj` u) $ \v -> do
      mu <- GM.read ms u
      GM.modify ms (onAct mu <>) v

  ms' <- U.unsafeFreeze ms
  U.iforM_ safe $ \i b ->
    unless b $ do
      printBSB $ (\(Acc ((!d, !_), (!d', !_))) -> d + d') $ ms' G.! i

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc429/tasks/abc429_e
main :: IO ()
main = runIO solve
