main=do n<-readLn;interact$(\(x,y)->unwords.map show$head[[i`div`n+1,i`mod`n+1]|(i,a,b)<-zip3[0..]x y,a/=b]).splitAt(n*n).filter(/='\n')
