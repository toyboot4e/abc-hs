#!/usr/bin/env stack

main :: IO ()
main = interact $ unlines . reverse . tail . lines
