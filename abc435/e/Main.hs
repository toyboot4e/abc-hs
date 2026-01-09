-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Unsafe.Coerce;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.DsuMonoid as DsuM;import qualified AtCoder.Extra.Graph as Gr;import qualified AtCoder.Extra.Vector as EV;import qualified AtCoder.Extra.Vector.Prim as EVP;import qualified AtCoder.Extra.Bisect as B;import qualified AtCoder.Extra.Math as EM;import qualified AtCoder.Extra.HashMap as EHM;import qualified AtCoder.Extra.IntMap as EIM;import qualified AtCoder.Extra.IntSet as EIS;import qualified AtCoder.Extra.IntervalMap as EIT;import AtCoder.Extra.Ix0;import qualified AtCoder.Extra.Monoid.RangeAdd as RangeAdd;import qualified AtCoder.Extra.Monoid.RangeSet as RangeSet;import qualified AtCoder.Extra.Monoid.RollingHash as RH;import qualified AtCoder.Extra.Semigroup.Matrix as Mat;import qualified AtCoder.Extra.Semigroup.Permutation as Permutation;import qualified AtCoder.Extra.Tree as Tr;import qualified AtCoder.Extra.Tree.Hld as Hld;import qualified AtCoder.Extra.Tree.Lct as Lct;import qualified AtCoder.Extra.Tree.TreeMonoid as Tm;import qualified AtCoder.FenwickTree as Ft;import qualified AtCoder.Internal.MinHeap as MH;import qualified AtCoder.Internal.Queue as Q;import qualified AtCoder.LazySegTree as LSeg;import qualified AtCoder.ModInt as MI;import qualified AtCoder.SegTree as Seg;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC

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

-- }}} toy-lib import
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- | @IntervalMap@ is a sparse map that manages non-overlapping @(l, r, x)@ value pairs.
newtype IntervalMap a = IntervalMap
  { -- | @l@ -> @(r, a)@
    unIM :: IM.IntMap (Int, a)
  }
  deriving newtype (Show, Eq)

-- | \(O(1)\) Creates an empty `IntervalMap`.
emptyIM :: IntervalMap a
emptyIM = IntervalMap IM.empty

-- | \(O(NW)\) Creates an interval map combining successive equal values into one.
fromVecMIM :: (G.Vector v a, Eq a, Monad m) => v a -> (Int -> Int -> a -> m ()) -> m (IntervalMap a)
fromVecMIM xs onAdd = fmap (IntervalMap . fst) $ foldM step (IM.empty, 0 :: Int) $ G.group xs
  where
    step (!map, !l) !xs' = do
      let !l' = l + G.length xs'
          !map' = IM.insert l (l' - 1, G.head xs') map
      onAdd l (l' - 1) (G.head xs')
      return (map', l')

-- | \(O(NW)\) Pure variant of `fromVecMIM`
fromVecIM :: (G.Vector v a, Eq a) => v a -> IntervalMap a
fromVecIM xs = runIdentity (fromVecMIM xs onAdd)
  where
    onAdd _ _ _ = pure ()

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@.
lookupIM :: Int -> Int -> IntervalMap a -> Maybe (Int, Int, a)
lookupIM l r (IntervalMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!l', (!r', !a))
        | r <= r' -> Just (l', r', a)
      _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readMayIM :: Int -> Int -> IntervalMap a -> Maybe a
readMayIM l r (IntervalMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !a))
        | r <= r' -> Just a
      _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readIM :: (HasCallStack) => Int -> Int -> IntervalMap a -> a
readIM l r rm = case readMayIM l r rm of
  Nothing -> error $ "[readIM] not a member: " ++ show (l, r)
  Just !a -> a

-- | \(O(\min(n, W))\) Boolean variant of `lookupIM`.
intersectsIM :: Int -> Int -> IntervalMap a -> Bool
intersectsIM l r (IntervalMap map)
  | r < l = False
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !_)) -> r <= r'
      _ -> False

-- | \(O(\min(n, W))\) Point variant of `intersectsIM`.
containsIM :: Int -> IntervalMap a -> Bool
containsIM i = intersectsIM i i

-- | Amortized \(O(\min(\log n, W))\) interval insertion with side effects. Old overlapping
-- intervals are overwritten.
insertMIM :: (Monad m, Eq a) => Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> IntervalMap a -> m (IntervalMap a)
insertMIM l0 r0 x onAdd onDel (IntervalMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  (!l', !r', !map') <- handleLeft l0 r map
  onAdd l' r' x
  let !map'' = IM.insert l' (r', x) map'
  return $! IntervalMap map''
  where
    handleRight l r map = case IM.lookupGE l map of
      Just interval0@(!_, (!_, !_)) -> run interval0 l r map
      Nothing -> return (r, map)

    -- Looks into intervals with @l' >= l0@.
    --           [----]
    -- (i)            *--------]   overwrite if it's x
    -- (ii)   [-------]*      delete anyways
    -- (iii)    *(------]     overwrite if it's x, or
    run (!l', (!r', !x')) l r map
      | l' > r + 1 = do
          -- not adjacent: end.
          return (r, map)
      -- (i)
      | l' == r + 1 && x' == x = do
          -- adjacent interval with the same value: merge into one.
          onDel (r + 1) r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | l' == r + 1 = do
          -- adjacent interval with different values: nothing to do.
          return (r, map)
      -- (ii)
      | r' <= r = do
          -- inside the interval: delete and continue
          onDel l' r' x'
          let !map' = IM.delete l' map
          -- TODO: wrap it (DRY)
          case IM.lookupGT l' map' of
            Just rng -> run rng l r map'
            Nothing -> return (r, map')
      -- (iii)
      | x' == x = do
          -- intersecting interval with the same value: merge into one.
          onDel l' r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | otherwise = do
          -- intersecting interval with a different value: delete the intersection.
          onDel l' r x'
          let !map' = IM.insert (r + 1) (r', x') $ IM.delete l' map
          return (r, map')

    handleLeft l r map = case IM.lookupLT l map of
      Nothing -> return (l, r, map)
      Just (!l', (!r', !x'))
        -- (i): adjacent interval
        | r' + 1 == l0 && x' == x -> do
            -- adjacent interval with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (l', r, map')
        | r' + 1 == l -> do
            -- adjacent interval with different values: nothing to do.
            return (l, r, map)
        -- (ii): not intersecting
        | r' < l -> do
            return (l, r, map)
        -- (iii): intersecting
        | x' == x -> do
            -- insersecting interval with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (min l l', max r r', map')
        | r' > r -> do
            -- intersecting interval with a different value: split into three.
            onDel l' r' x'
            onAdd l' (l - 1) x'
            onAdd (r + 1) r' x'
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')
        | otherwise -> do
            -- insersecting interval with a different value: delete.
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')

-- | Amortized \(O(\min(\log n, W))\) interval insertion. Old overlapping intervals are overwritten.
insertIM :: (Eq a) => Int -> Int -> a -> IntervalMap a -> IntervalMap a
insertIM l r x rm = runIdentity (insertMIM l r x onAdd onDel rm)
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

-- | Amortized \(O(\min(\log n, W))\) interval deletion with side effects.
deleteMIM :: (Monad m) => Int -> Int -> (Int -> Int -> a -> m ()) -> IntervalMap a -> m (IntervalMap a)
deleteMIM l0 r0 onDel (IntervalMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  !map' <- handleLeft l0 r map
  return $ IntervalMap map'
  where
    handleRight l r map = case IM.lookupGE l map of
      Just interval0@(!_, (!_, !_)) -> run interval0 l r map
      Nothing -> return (r, map)

    run (!l', (!r', !x')) l r map
      | l' >= r + 1 = do
          return (r, map)
      | r' <= r = do
          onDel l' r' x'
          let !map' = IM.delete l' map
          case IM.lookupGT l' map' of
            Just rng -> run rng l r map'
            Nothing -> return (r, map')
      | otherwise = do
          onDel l' r x'
          let !map' = IM.insert (r + 1) (r', x') $ IM.delete l' map
          return (r, map')

    handleLeft l r map = case IM.lookupLT l map of
      Nothing -> return map
      Just (!l', (!r', !x'))
        | r' < l -> do
            return map
        | r' > r -> do
            onDel l' r' x'
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') $ IM.delete l' map
            return map'
        | otherwise -> do
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return map'

-- | Amortized \(O(\min(\log n, W))\) interval deletion.
deleteIM :: Int -> Int -> IntervalMap a -> IntervalMap a
deleteIM l r rm = runIdentity (deleteMIM l r onDel rm)
  where
    onDel _ _ _ = pure ()

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2P
  !lrs <- U.replicateM q ints11P

  cnt <- UM.replicate 1 (0 :: Int)

  let onAdd l r _ = do
        GM.modify cnt (+ (r + 1 - l)) 0
  let onDel l r _ = do
        GM.modify cnt (subtract (r + 1 - l)) 0

  U.foldM'_
    (\(im :: IntervalMap Int) (!l, !r) -> do
        im' <- insertMIM l r (1 :: Int) onAdd onDel im
        printBSB . (n -) =<< GM.read cnt 0
        pure im'
    )
    emptyIM
    lrs

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc435/tasks/abc435_e
main :: IO ()
main = runIO solve
