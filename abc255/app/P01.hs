module Main (main) where

main :: IO ()
main = do
  [r, c] <- map read . words <$> getLine :: IO [Int]
  [a11, a12] <- map read . words <$> getLine :: IO [Int]
  [a21, a22] <- map read . words <$> getLine :: IO [Int]

  -- FIXME: prefer expr-oriented code
  case (r, c) of
    (1, 1) -> print a11
    (1, 2) -> print a12
    (2, 1) -> print a21
    (2, 2) -> print a22
    _ -> error "unrechable"

