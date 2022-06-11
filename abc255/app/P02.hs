module Main (main) where

import Control.Monad
import GHC.Float

-- TODO: Int Float cast
getDist :: [Int] -> [Int] -> Float
getDist [x1, y1] [x2, y2] = sqrt $ pow2 (x2 - x1) + pow2 (y2 - y1)
  where
    pow2 x = int2Float $ x * x
getDist _ _ = undefined

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]

  -- light point indices
  as <- map (\x -> read x - 1) . words <$> getLine :: IO [Int]

  -- people's coordintes
  xys <- replicateM n (map read . words <$> getLine :: IO [Int])

  let minDs =
        [ minimum ds
          | i <- [0 .. n -1],
            i `notElem` as,
            let xy = xys !! i,
            let ds = [d | a <- as, let d = getDist (xys !! a) xy]
        ]

  print $ maximum minDs
