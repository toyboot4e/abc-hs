main=interact$show.f.map read.words;f(_:h:x)=head$[i|(i,y)<-zip[2..]x,y+0>h]++[-1]
