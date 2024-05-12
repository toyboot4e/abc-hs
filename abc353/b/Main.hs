#!/usr/bin/env stack
{- stack script --resolver lts-21.7 --package array,bitvec,bytestring,containers,deepseq,extra,hashable,unordered-containers,heaps,mtl,utility-ht,vector,vector-algorithms,primitive,QuickCheck,random,time,transformers --ghc-options "-D DEBUG" -}

main=interact$show.(0%).tail.map read.words
a%[_]=min 1a
a%(k:y:r)|a+y>k=1+0%(k:y:r)|0<1=(a+y)%(k:r)
