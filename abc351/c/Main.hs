main=interact$show.f[-1].tail.map read.words
f(x:y:r)n|x==y=f(x+1:r)n
f x[]=length x-1
f r(i:n)=f(i:r)n
