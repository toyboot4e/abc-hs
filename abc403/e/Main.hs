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

debug :: Bool ; debug = False

{- ORMOLU_ENABLE -}

data Trie k v = Trie
  { -- keyT :: {-# UNPACK #-} Int,
    payloadT :: !v,
    childrenT :: !(M.Map k (Trie k v))
  }
  deriving (Show, Eq)

-- | \(O(1)\)
rootT :: (Ord k) => v -> Trie k v
rootT x = Trie x M.empty

-- | \(O(k \log w)\)
lookupT :: forall k v. (Ord k) => [k] -> Trie k v -> Maybe v
lookupT = inner
  where
    inner :: [k] -> Trie k v -> Maybe v
    inner [] !trie = Just $ payloadT trie
    inner (k : keys) !trie = do
      child <- M.lookup k $ childrenT trie
      inner keys child

-- | \(O(k \log w)\)
memberT :: forall k v. (Ord k) => [k] -> Trie k v -> Bool
memberT ks = isJust . lookupT ks

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modifyT :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modifyT f = modifyNodeT (\(Trie p v) -> Trie (f p) v)

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modifyNodeT :: forall k v. (Ord k) => (Trie k v -> Trie k v) -> [k] -> Trie k v -> Trie k v
modifyNodeT f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = f trie
    inner (k : keys) trie = trie {childrenT = M.adjust (inner keys) k (childrenT trie)}

-- TODO: deduplicate the implementations of alloc*

-- | \(O(k \log w)\) Non-existing nodes in the path will have the same payload.
allocPathT :: forall k v. (Ord k) => [k] -> v -> v -> Trie k v -> Trie k v
allocPathT keys0 vParent vLeaf = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie
    inner (k : keys) (Trie payload children) =
      -- TODO: fix it to one-pass
      case M.lookup k children of
        Just child -> Trie payload $! M.insert k (inner keys child) children
        Nothing -> Trie payload $! M.insert k (allocPath keys) children
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie vLeaf M.empty
    allocPath (k : keys) = Trie vParent $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the
-- target node.
allocModifyT :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModifyT f keys0 vDefault = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie { payloadT = f (payloadT trie) }
    inner (k : keys) (Trie payload children) =
      -- TODO: fix it to one-pass
      case M.lookup k children of
        Just child -> Trie payload $! M.insert k (inner keys child) children
        Nothing -> Trie payload $! M.insert k (allocPath keys) children
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie (f vDefault) M.empty
    allocPath (k : keys) = Trie vDefault $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the node
-- in the path.
allocModifyPathT :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModifyPathT f keys0 vDefault = inner keys0
  where
    !fv = f vDefault
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie { payloadT = f (payloadT trie) }
    inner (k : keys) (Trie payload children) =
      case M.lookup k children of
        Just child -> Trie payload' $! M.insert k (inner keys child) children
        Nothing -> Trie payload' $! M.insert k (allocPath keys) children
      where
        !payload' = f payload
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie fv M.empty
    allocPath (k : keys) = Trie fv $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Modifies payload of a node and their parents.
--
-- ==== Constraints
-- - The key must be in the map.
modifyPathT :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modifyPathT f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] !trie = trie {payloadT = f (payloadT trie)}
    inner (k : keys) !trie = Trie (f (payloadT trie)) (M.adjust (inner keys) k (childrenT trie))

-- | \(O(k \log w)\) Returns prefix payloads, excluding the root.
pathT :: forall k v. (Ord k) => [k] -> Trie k v -> [v]
pathT = inner
  where
    inner :: [k] -> Trie k v -> [v]
    inner [] !trie = []
    inner (k : keys) !trie = do
      -- TODO: write monadic code
      case M.lookup k (childrenT trie) of
        Just child -> (payloadT child :) $ inner keys child
        Nothing -> []

data Payload = Payload
  { -- | Represents whether the prefix is acceptable.
    acceptableP :: !Bool,
    -- | Query indices that match to the prefix.
    matchesP :: !IS.IntSet
  }
  deriving (Show, Eq)

insertP :: Int -> Payload -> Payload
insertP k (Payload b is) = Payload b $! IS.insert k is

-- | Rolling hash answer
solve :: StateT BS.ByteString IO ()
solve = do
  !q <- int'
  !qs <- V.replicateM q $ (,) <$> int' <*> word'

  let !res = V.tail $ V.iscanl' step s0 qs
        where
          !s0 = (rootT @Char @Payload (Payload False IS.empty), IS.empty)
          !p0 = Payload False IS.empty
          !p1 = Payload True IS.empty

          -- add prefix
          step :: Int -> (Trie Char Payload, IS.IntSet) -> (Int, BS.ByteString) -> (Trie Char Payload, IS.IntSet)
          step iQuery (!trie, !rest) (1, !s) = (trie', rest')
            where
              !trie' = allocModifyT (const p1) (BS.unpack s) (Payload False IS.empty) trie
              !rest' = fromMaybe rest $ do
                payload <- lookupT (BS.unpack s) trie
                pure $! IS.difference rest $ matchesP payload

          -- add target
          step iQuery (!trie, !rest) (2, !s) = (trie', rest')
            where
              !anyMatch = any acceptableP $ pathT (BS.unpack s) trie
              !trie'
                | anyMatch = trie
                | otherwise = allocModifyPathT (insertP iQuery) (BS.unpack s) p0 trie
              !rest'
                | anyMatch = rest
                | otherwise = IS.insert iQuery rest
          step _ _ _ = error "unreachable"

  printBSB $ unlinesBSB $ V.imap (\i (!_, !rest) -> IS.size rest) res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc403/tasks/abc403_e
main :: IO ()
main = runIO solve
