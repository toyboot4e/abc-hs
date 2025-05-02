-- toy-lib: https://github.com/toyboot4e/toy-lib
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT(..), evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import Data.Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Radix as VAR;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC;import Unsafe.Coerce;
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
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

pattern ONES, ADD, MUL :: Int
pattern ONES = 0
pattern MUL = 1
-- ADD is not preferable, so the last
pattern ADD = 2

isOnes 1 = True
isOnes 11 = True
isOnes 111 = True
isOnes 1111 = True
isOnes _ = False

-- | ONES and MUL don't need parentheses on MUL!
mulLen :: Int -> Int -> Int -> Int -> Int
mulLen l1 pat1 l2 pat2
  -- LHS * (RHS)
  | pat1 /= ADD && pat2 /= ADD = l1 + 1 + l2
  | pat1 /= ADD || pat2 /= ADD = l1 + 3 + l2
  -- (LHS) * (RHS)
  | otherwise = l1 + 5 + l2

solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'

  -- (4, kind, from1, from2)
  let res = U.constructN @(Int, Int, Int, Int) (n + 1) $ \sofar -> case G.length sofar of
        0 -> (-1, -1, -1, -1)
        1 -> (1, ONES, -1, -1)
        11 -> (2, ONES, -1, -1)
        111 -> (3, ONES, -1, -1)
        1111 -> (4, ONES, -1, -1)
        len -> select $ mapMaybe evalMul [2 .. len `div` 2] ++ map evalAdd [1 .. len `div` 2]
          where
            select [] = error "unreachable??"
            select xs = minimumBy (comparing (\(!x, !pat, !_, !_) -> (x, pat))) xs
            evalAdd x =
              let !y = len - x
                  (!lx, !_, !_, !_) = sofar G.! x
                  (!ly, !_, !_, !_) = sofar G.! y
               in (lx + ly + 1, ADD, x, y)
            evalMul x
              | r /= 0 = Nothing
              | otherwise =
                  let (!lx, !patX, !_, !_) = sofar G.! x
                      (!ly, !patY, !_, !_) = sofar G.! y
                      !theL = mulLen lx patX ly patY
                   in Just (theL, MUL, x, y)
              where
                (!y, !r) = len `divMod` x

  memo <- VM.replicate (n + 1) $ mempty @BSB.Builder
  done <- UM.replicate (n + 1) False

  let recons 1 = pure $ BSB.string7 "1"
      recons 11 = pure $ BSB.string7 "11"
      recons 111 = pure $ BSB.string7 "111"
      recons 1111 = pure $ BSB.string7 "1111"
      recons i = do
        b <- GM.exchange done i True
        if b
          then GM.read memo i
          else do
            let (!_len, !pat, !lhs, !rhs) = res G.! i
            bsbL <- recons lhs
            bsbR <- recons rhs
            let (!_, !pat1, !_, !_) = res G.! lhs
            let (!_, !pat2, !_, !_) = res G.! rhs
            let !bsb = case pat of
                  ADD -> bsbL <> showBSB '+' <> bsbR
                  MUL
                    | pat1 /= ADD && pat2 /= ADD -> bsbL <> showBSB '*' <> showBSB bsbR
                    | pat2 /= ADD -> showBSB '(' <> bsbL <> showBSB ")*" <> bsbR
                    | pat1 /= ADD -> bsbL <> showBSB "*(" <> bsbR <> showBSB ')'
                    -- (LHS) * (RHS)
                    | otherwise -> showBSB '(' <> bsbL <> showBSB ")*(" <> bsbR <> showBSB ")"
            GM.write memo i bsb
            pure bsb

  let !_ = dbg $ res G.! n
  printBSB =<< recons n

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc403/tasks/abc403_e
main :: IO ()
main = runIO solve
