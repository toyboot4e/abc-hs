#!/usr/bin/env stack
{- stack script --resolver lts-21.6 --package array --package bitvec --package bytestring --package containers --package deepseq --package extra --package hashable --package unordered-containers --package heaps --package mtl --package utility-ht --package vector --package vector-algorithms --package primitive --package QuickCheck --package random --package time --package transformers --ghc-options "-D DEBUG" -}

main :: IO ()
main=do n<-readLn;interact$(\(x,y)->unwords.map show$head[[i`div`n+1,i`mod`n+1]|(i,(a,b))<-zip[0..](zip x y),a/=b]).splitAt(n*n).filter(/='\n')

