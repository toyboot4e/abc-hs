#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.IO

getLineInt :: IO Int
getLineInt = fst . fromJust . BS.readInt <$> BS.getLine

intListToBsb :: BSB.Builder -> [Int] -> BSB.Builder
intListToBsb c xs = mconcat $ intersperse c $ map BSB.intDec xs

main :: IO ()
main = do
  xs <- replicateM 10 getLineInt

  -- mapM_ $ print $ solve xs [1, 2]

  let bs = map (intListToBsb $ BSB.char7 ' ') xs
   in BSB.hPutBuilder stdout $
        (mconcat $ intersperse (BSB.char7 '\n') bs) <> (BSB.char7 '\n')

  -- let bs = map (\xy -> mconcat $ intersperse (BSB.char7 ' ') (map BSB.intDec xy)) res
  -- BSB.hPutBuilder stdout $
  --   (mconcat $ intersperse (BSB.char7 '\n') bs) <> (BSB.char7 '\n')

  return ()

solve :: Int -> Int -> Int
solve a b = undefined

-- VU.generate n id
-- VU.unfoldnN n BS.uncons <$> BS.getLine

-- main :: IO ()
-- main = do
--   n <- getLineInt
--   rs <- replicateM n getLineInts
--   let res = solve rs
--       bs = map (intListToBsb $ BSB.char7 ' ') res
--     in BSB.hPutBuilder stdout $
--       (mconcat $ intersperse (BSB.char7 '\n') $ bs) <> (BSB.char7 '\n')

