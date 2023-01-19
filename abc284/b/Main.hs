#!/usr/bin/env stack

import Control.Monad
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [nTests] <- getLineIntList
  replicateM_ nTests solve

solve :: IO ()
solve = do
  [_n] <- getLineIntList
  xs <- getLineIntList
  print . length $ filter odd xs

