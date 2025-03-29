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

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

-- 互いに疎
testMutEx :: V.Vector (S.Set Int) -> Bool
testMutEx sets = snd $ V.ifoldl' step (S.empty, True) sets
  where
    step (!set, !result) nextChar nextSet = (set', result')
      where
        result' = result && S.disjoint set nextSet
        set' = S.union set nextSet

evalSpan :: Int -> S.Set Int -> Int
evalSpan target set
  | S.size set == 0 = 0
  | S.member target set = S.size set - 1
  | otherwise = S.size set

solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !s <- line'
  !t <- line'

  -- アルファベット毎に一致させると考える
  -- - それぞれの { 区間集合の中の文字 } が互いに独立であることが必要条件
  -- - 区間集合が持つ文字の集合の要素数 - 1 が基本的な答え
  -- - 区間集合が目標の文字を持っていなければ操作回数 +1
  --
  -- サイクルになっている場合は一時退避が必要なのか
  --
  -- サイクルの数を数えて操作回数に足す
  -- ただし避難先が無い場合は失敗
  -- - 1 個でも余った文字があれば OK, なのか？
  --
  -- サイクルの処理をどう考えるか
  -- どっかの文字に寄せて道を譲ることもできそう

  let rleT = rleOf t
  let offsetsT = U.scanl' (+) (0 :: Int) . U.fromList $ map snd rleT
  let ordC = subtract (ord 'a') . ord
  let charSpans =
        V.accumulate (flip (:)) (V.replicate 26 [])
          . V.imap (\i (!c, !_) -> (ordC c, (offsetsT G.! i, offsetsT G.! (i + 1))))
          . U.convert
          $ U.fromList rleT

  -- target char -> set of charaters there
  let charSetsS =
        V.accumulate S.union (V.replicate 26 S.empty) $
          V.imap
            ( \i lrs ->
                ( i,
                  foldl' S.union S.empty $
                    map (\(!l, !rEx) -> S.fromList $ map (ordC . BS.index s) [l .. rEx - 1]) lrs
                )
            )
            charSpans

  unless (testMutEx charSetsS) $ do
    let !_ = dbg ("not ex")
    -- 目標の異なる 2 文字が連動して変化する
    printBSB "-1"
    liftIO exitSuccess

  -- functional graph を作る
  let es =
        U.convert
          . V.mapMaybe
            ( \(!target, !set) ->
                -- FIXME: 2 要素以上なら本当に道が開くのか？
                if S.member target set || S.size set /= 1 {- null or more than one -}
                  then Nothing
                  else Just (target, head (S.toList set))
            )
          $ V.indexed charSetsS

  -- TODO: solve declartively
  let cycles = topSccSG $ buildSG_ 26 es
  subCnt <- UM.replicate 1 (0 :: Int)
  for_ cycles $ \vs -> do
    -- when (length vs == 26) $ do
    --   -- サイクルを解消できないため解決不能
    --   let !_ = dbg ("incompatible cycle")
    --   printBSB "-1"
    --   liftIO exitSuccess
    when (length vs > 1) $ do
      -- 閉路である
      GM.modify subCnt (+ 1) 0
  dn <- GM.read subCnt 0

  -- 1 つでも閉路があって退避不可なら解けない
  when (dn > 0) $ do
    let size = length . filter ((== 1) . S.size) . V.toList $ charSetsS
    when (size == 26) $ do
      printBSB "-1"
      liftIO exitSuccess

  let !_ = dbg (dn, charSetsS)
  printBSB $ (+ dn) . V.sum $ V.imap evalSpan charSetsS

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc399/tasks/abc399_e
main :: IO ()
main = runIO solve
