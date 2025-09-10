get.hilo<-function(x) {
  med<-median(x[x!=0])
  hilo<-rep(0,length(x))
  hilo[x>med] <- 2
  hilo[x<=med] <- 1
  hilo[x==0] <- 0
  return(hilo)
}