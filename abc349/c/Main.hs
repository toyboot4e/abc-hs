import Data.Char
import Data.List
main=interact$g.f.map (map toLower.reverse).words
f[s,('x':t)]=isSubsequenceOf t s
f[s,t]=isSubsequenceOf t s
g True="Yes"
g _="No"
