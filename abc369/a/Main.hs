import Data.List;main=interact$f.map read.words;f[a,b]=show$sum[1|x<-[-300..300],(==1).length.nub.(zipWith(-)<*>tail)$sort[a,b,x]]
