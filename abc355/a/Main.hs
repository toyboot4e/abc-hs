main=interact$show.f.map read.words
f[a,b]|a==b=(-1)|0<1=[i|i<-[1..3],notElem i[a,b]]!!0
