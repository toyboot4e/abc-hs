main = do
  [n,m] <- map readInt . words <$> getLine
  ass <- linToMat n m <$> getVecURest (n*m) rInt
  let bmp = runST $ do
        !bmp <- V.replicateM n $ VUM.replicate n (Bit False)
        !valmp <- V.replicateM 1000 $ VUM.replicate n (Bit False)
        VU.forM_ (VU.generate m id) $ \ !j -> do
          V.imapM_  (\ !i !vmp -> VUM.write vmp i (Bit True))
            $ V.unsafeBackpermute valmp (V.map (`VU.unsafeIndex` j) ass)
          V.iforM_ (V.unsafeBackpermute valmp (V.map (`VU.unsafeIndex` j) ass))
            $ \ !i !vmp -> do
            !vmp <- VU.unsafeFreeze vmp
            Bit.zipInPlace xor vmp (bmp `V.unsafeIndex` i)
          V.imapM_  (\ !i !vmp -> VUM.write vmp i (Bit False) )
            $ V.unsafeBackpermute valmp $ V.map (`VU.unsafeIndex` j) ass
        V.mapM VU.unsafeFreeze bmp
      resdouble = V.sum $ V.map popCount bmp
      res = (resdouble - if odd m then n else 0) .>>. 1
  print res
  VU.forM_ (VU.generate m id) $ \ !j -> do
          V.imapM_  (\ !i !vmp -> VUM.write vmp i (Bit True))
            $ V.unsafeBackpermute valmp (V.map (`VU.unsafeIndex` j) ass)
          V.iforM_ (V.unsafeBackpermute valmp (V.map (`VU.unsafeIndex` j) ass))
            $ \ !i !vmp -> do
            !vmp <- VU.unsafeFreeze vmp
            Bit.zipInPlace xor vmp (bmp `V.unsafeIndex` i)
