import Data.List;main=interact$show.f.map read.words;f(n:r)=sum.map(sum.map snd.init).groupBy(\(i,_)(j,_)->i==j).sort.zipWith(,)r$drop n r
