module Main (main) where

solve :: [Int] -> Int
solve xs = undefined

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  print $ solve xs
