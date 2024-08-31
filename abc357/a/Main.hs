main=interact$show.f.map read.words;f(_:m:r)=length.takeWhile(<=m)$scanl1(+)r
