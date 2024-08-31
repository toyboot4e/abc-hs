main=interact$unwords.((\k->map(show.(`div`k)).filter((==0).(`mod`k)))<$>head<*>tail).tail.map read.words
