import Data.List
main=interact$show.f.map read.words;f(n:t:p:l)=max 0$t-sort l!!(n-p)
