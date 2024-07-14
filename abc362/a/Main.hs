#!/usr/bin/env stack
{- stack script --resolver lts-21.11 --package array,bitvec,bytestring,containers,deepseq,extra,hashable,unordered-containers,heaps,mtl,utility-ht,vector,vector-algorithms,primitive,QuickCheck,random,time,transformers --ghc-options "-D DEBUG" -}

-- verification-helper: PROBLEM <<url>>
main :: IO ()
main=interact$show.(f.map read.init<*>last).words;f[r,g,b](c:_)|c=='R'=0+min g b|c=='G'=min b r|0<1=min r g

