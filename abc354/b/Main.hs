import Data.List
main=interact$(g<*>sort.f<*>(0%).tail).words
s%[]=s
s%(_:b:r)=s+read b%r
f(_:n:r)=n:f r
f _=[]
g(n:_)x t=x!!(t`mod`read n)
