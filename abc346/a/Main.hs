main=interact$unwords.(zipWith((show.).(*))<*>tail).tail.map read.words
