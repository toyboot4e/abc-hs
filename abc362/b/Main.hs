#!/usr/bin/env stack
{- stack script --resolver lts-21.11 --package array,bitvec,bytestring,containers,deepseq,extra,hashable,unordered-containers,heaps,mtl,utility-ht,vector,vector-algorithms,primitive,QuickCheck,random,time,transformers --ghc-options "-D DEBUG" -}

-- verification-helper: PROBLEM <<url>>
main :: IO ()
main=interact$y.((==)<$>(2*).maximum<*>sum).f.map read.words;f[]=[];f(x:y:r)=x*x+y*y:f r;y True="Yes";y _="No"

