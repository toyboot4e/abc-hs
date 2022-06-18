#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

module Main (main) where

import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

bsToInts :: BS.ByteString -> [Int]
bsToInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineInts :: IO [Int]
getLineInts = bsToInts <$> BS.getLine

main :: IO ()
main = do
  xs <- getLineInts
  print $ solve xs

solve :: [Int] -> Int
solve xs = undefined
