import Data.List
main=interact$(\b->if b then"Yes"else"No").all((==2).length).group.sort.map length.group.sort.init
