module Main (main) where

solve :: Int -> Int -> Int -> Int -> Int
solve x a d n
  | d >= 0 && x <= l = abs $ l - x
  | d >= 0 && x >= r = abs $ r - x
  | d <= 0 && x <= r = abs $ r - x
  | d <= 0 && x >= l = abs $ l - x
  | otherwise =
    minimum
      [ abs (x - (a + d * (n - 1))),
        abs (x - (a + d * i)),
        abs (x - (a + d * (i + 1))),
        abs (x - (a + d * (i - 1)))
      ]
  where
    l = a
    r = a + d * (n - 1)
    i = (x - a) `div` d

main :: IO ()
main = do
  [x, a, d, n] <- map read . words <$> getLine :: IO [Int]
  print $ solve x a d n
