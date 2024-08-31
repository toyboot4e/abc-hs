import Data.Char
main=interact$flip map<*>f.sum.map(g.isUpper).init
g b|b=1|0<1=(-1)
f n|n>0=toUpper|1>0=toLower
