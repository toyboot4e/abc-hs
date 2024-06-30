import Data.List.Extra;main=interact$f.words;f[s,t]|elem t$concatMap(transpose.(`chunksOf`s))[1..length s-1]="Yes"|0<1="No"
