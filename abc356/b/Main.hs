#!/usr/bin/env stack
{- stack script --resolver lts-21.7 --package array,bitvec,bytestring,containers,deepseq,extra,hashable,unordered-containers,heaps,mtl,utility-ht,vector,vector-algorithms,primitive,QuickCheck,random,time,transformers --ghc-options "-D DEBUG" -}

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc356/tasks/abc356_b
main :: IO ()
main=interact$f.map read.words
f(n:m:r)=y.and$zipWith(<=)(take m r)$foldl(zipWith(+))[0|_<-[1..m]]$take n.tail$iterate(drop m)r
y True="Yes"
y False="No"
