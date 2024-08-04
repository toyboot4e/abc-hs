import Data.List;main=interact$show.snd.last.init.sort.(`zip`[1..]).tail.map (read @Int).words
