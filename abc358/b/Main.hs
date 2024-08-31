#!/usr/bin/env stack
{- stack script --resolver lts-21.10 --package array,bitvec,bytestring,containers,deepseq,extra,hashable,unordered-containers,heaps,mtl,utility-ht,vector,vector-algorithms,primitive,QuickCheck,random,time,transformers --ghc-options "-D DEBUG" -}

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc358/tasks/abc358_b
main :: IO ()
main=interact$unwords.map show.f.map read.words;f(n:a:t)=tail$scanl(\x y->max x y+a)0 t

