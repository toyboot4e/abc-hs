main=interact$(\x->unwords.map show$(length x-1:).concat$zipWith(\a b->[a,b])x(tail x)).h.map read.words
n j r 0=(2^).last$takeWhile((<=r).(2^))j
n j r l=(\x->if x==[]then r+1 else l+2^last x)$takeWhile(\i->l+2^i<=r&&l`mod`2^i==0)j
h[l,r]=takeWhile(<=r)$iterate(n[0..]r)l
