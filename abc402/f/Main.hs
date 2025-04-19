-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
-- {{{ toy-lib import
import ToyLib.Contest.Prelude
-- import ToyLib.Contest.Bisect
-- import ToyLib.Contest.Graph
import ToyLib.Contest.Grid
-- import ToyLib.Contest.Tree

-- import Data.BitSet
import Data.Buffer
-- import Data.Core.SemigroupAction
-- import Data.ModInt
-- import Data.PowMod
-- import Data.Primes
import Math.PowMod

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

calcRoutes :: U.Vector Int -> Int -> Int -> IxUVector (Int, Int) Int -> V.Vector (U.Vector Int)
calcRoutes digits n m gr = runST $ do
  let !_ = dbg ("a", n, m, gr)
  let !fms = U.generate n (`factMod` m)
  bufs <- (IxVector (zero2 n n) <$>) $ do
    let !_ = dbg ("xx", n, m, gr)
    V.generateM (n * n) $ \i -> do
      let (!y, !x) = i `divMod` n
      let !s = y + x
      let !_ = dbg ("i", (i, s), s < n, n, m)
      if s < n
        then newBuffer (fms G.! (n - 1) `div` (fms G.! (n - 1 - s) * fms G.! s))
        else newBuffer 0

  let !_ = dbg ("b", gr)
  -- mark the starting point
  pushBack (bufs @! (0, 0)) 0

  -- spread!
  for_ [0 .. n - 1] $ \y -> do
    for_ [0 .. n - 1] $ \x -> do
      let s = y + x
      when (s < (n - 1)) $ do
        froms <- unsafeFreezeBuffer $ bufs @! (y, x)
        let !d = mulMod m  (digits G.! s) (gr @! (y, x))
        U.forM_ froms $ \from -> do
          let !next = addMod m d from
          pushBack (bufs @! (y + 1, x)) next
          pushBack (bufs @! (y, x + 1)) next

  -- collect from right up to left down
  !bufs' <- IxVector (zero2 n n) <$> V.mapM unsafeFreezeBuffer (vecIV bufs)

  let !_ = dbg ("go?", n, m)
  let !_ = dbgGrid bufs'
    -- pure ()

  pure $ V.generate n $ \i -> bufs' @! (n - 1 - i, i)
  where
    !_ = dbg ("calc",n,m)

matchMax :: Int -> U.Vector Int -> U.Vector Int -> Int
matchMax m xs0 ys0 = U.maximum $ U.map f xs0
  where
    !ys = IS.fromList $ U.toList ys0
    -- TODO: lookupMax not in containers yet?
    !maxY = fromJust $ IS.lookupLT m ys
    f x = max cand1 cand2
      where
        !target = m - x
        !cand1 = case IS.lookupLT (m - x) ys of
          Nothing -> 0
          Just a -> x + a
        !cand2 = addMod m x maxY

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !gr <- mapIV digitToInt <$> getGrid' n n
  let !gr' = IxVector (zero2 n n) $ U.create $ do
        vec <- IxVector (zero2 n n) <$> UM.replicate (n * n) (-1 :: Int)
        for_ [0 .. n - 1] $ \y -> do
          for_ [0 .. n - 1] $ \x -> do
            let !y' = (n - 1) - x
            let !x' = (n - 1) - y
            writeIV vec (y, x) $ gr @! (y', x')
        pure $ vecIV vec

  let !digitsAll = U.constructN (2 * n - 1) $ \vec ->
        case U.length vec of
          0 -> 1
          i -> 10 * vec G.! (i - 1) `mod` m

  let !digits = U.take n $ U.reverse digitsAll
  let !digits' = U.take n digitsAll

  let !_ = dbg ("rs", n, m, gr')
  let !rs = calcRoutes digits' n m gr'

  let !_ = dbg ("ls", n, m, gr)
  let !ls = addDiags $ calcRoutes digits n m gr
        where
          addDiags = V.imap (\i -> U.map (addMod m (mulMod m (digits G.! (n - 1)) (gr @! (n - 1 - i, i)))))

  when (n < 10) $ do
    let !_ = note "ls" ls
    let !_ = note "rs" rs
    pure ()
  printBSB $ V.maximum $ V.zipWith (matchMax m) ls rs

  pure ()

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc402/tasks/abc402_f
main :: IO ()
main = runIO solve
