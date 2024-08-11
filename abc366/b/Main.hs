import Data.List;main=interact$unlines.map(dropWhileEnd(=='*')).transpose.map(++replicate 99'*').reverse.tail.lines
