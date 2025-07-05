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
import Data.UnionFind.Mutable

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- N + 2M の頂点
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !uvs <- U.replicateM m ints11'
  q <- int'
  qs <- U.map pred <$> intsU'

  uf <- newMUF $ n + 2 

  -- out going edges
  es <- VM.replicate n S.empty
  U.forM_ uvs $ \(!u, !v) -> do
    GM.modify es (S.insert v) u
    GM.modify es (S.insert u) v
  members <- VM.generate n S.singleton

  -- initial <- G.freeze es

  U.foldM'_
    ( \cnt iEdge -> do
        let (!u, !v) = uvs G.! iEdge
        r1 <- rootMUF uf u
        r2 <- rootMUF uf v

        b <- unifyMUF uf u v

        if not b
          then do
            let !_ = dbg ((u, v), "none")
            printBSB cnt
            pure cnt
          else do
            r' <- rootMUF uf u

            set1 <- GM.read es r1
            member1 <- GM.read members r1
            set2 <- GM.read es r2
            member2 <- GM.read members r2
            GM.write members r' $! S.union member1 member2

            let !set1' = S.difference set1 member2
            let !set2' = S.difference set2 member1
            let !set' = S.union set1' set2'
            GM.write es r' set'
            let !_ = dbg ((u, v), set1', set2', set')

            let !cnt' = cnt - 1 - (S.size set1' + S.size set2' - S.size set')
            let !_ = dbg (S.size set1, S.size set2, S.size set')

            printBSB cnt'
            pure cnt'
    )
    m
    qs

---- 時間を遡る
--solve :: StateT BS.ByteString IO ()
--solve = do
--  (!n, !m) <- ints2'
--  !uvs <- U.replicateM m ints11'
--  q <- int'
--  qs <- U.map pred <$> intsU'
--
--  uf <- newMUF n
--
--  -- incoming edges
--  es <- VM.replicate n S.empty
--  U.forM_ uvs $ \(!u, !v) -> do
--    GM.modify es (S.insert v) u
--    GM.modify es (S.insert u) v
--
--  -- 最終面
--  U.forM_ qs $ \iEdge -> do
--    let (!u, !v) = uvs G.! iEdge
--    r1 <- rootMUF uf u
--    r2 <- rootMUF uf v
--    set1 <- GM.read es r1
--    set2 <- GM.read es r2
--
--    whenM (unifyMUF uf u v) $ do
--      r' <- rootMUF uf u
--      GM.write es r' $! S.union set1 set2
--
--  -- 内部的な辺を全削除する
--  roots <- U.uniq . U.modify VAI.sort <$> U.generateM n (rootMUF uf)
--
--  printBSB "TODO"

-- solve :: StateT BS.ByteString IO ()
-- solve = do
--   (!n, !m) <- ints2'
--   !uvs <- U.replicateM m ints11'
--   q <- int'
--   qs <- U.map pred <$> intsU'
-- 
--   uf <- newMUF n
-- 
--   -- out going edges
--   es <- VM.replicate n S.empty
--   U.forM_ uvs $ \(!u, !v) -> do
--     GM.modify es (S.insert v) u
--     GM.modify es (S.insert u) v
--   members <- VM.generate n S.singleton
-- 
--   -- initial <- G.freeze es
-- 
--   U.foldM'_
--     ( \cnt iEdge -> do
--         let (!u, !v) = uvs G.! iEdge
--         r1 <- rootMUF uf u
--         r2 <- rootMUF uf v
-- 
--         b <- unifyMUF uf u v
-- 
--         if not b
--           then do
--             let !_ = dbg ((u, v), "none")
--             printBSB cnt
--             pure cnt
--           else do
--             r' <- rootMUF uf u
-- 
--             set1 <- GM.read es r1
--             member1 <- GM.read members r1
--             set2 <- GM.read es r2
--             member2 <- GM.read members r2
--             GM.write members r' $! S.union member1 member2
-- 
--             let !set1' = S.difference set1 member2
--             let !set2' = S.difference set2 member1
--             let !set' = S.union set1' set2'
--             GM.write es r' set'
--             let !_ = dbg ((u, v), set1', set2', set')
-- 
--             let !cnt' = cnt - 1 - (S.size set1' + S.size set2' - S.size set')
--             let !_ = dbg (S.size set1, S.size set2, S.size set')
-- 
--             printBSB cnt'
--             pure cnt'
--     )
--     m
--     qs

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc411/tasks/abc411_e
main :: IO ()
main = runIO solve
