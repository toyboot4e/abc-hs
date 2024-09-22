main=interact$show.f.map(map(pred.read).words).lines;f([n]:x)=foldl(\i j->x!!max i j!!min i j)(x!!0!!0)[1..n]+1
