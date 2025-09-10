get.quantiles<-function(x,n) {
  vec <- x
  cat <- rep(NA,length(vec))
  cat[vec==0] <- 0
  vec_nonzero<-vec[vec>0]
  breaks <- quantile(vec_nonzero, probs = seq(0, 1, length.out = n), na.rm = TRUE)
  cat[vec > 0] <- cut(
    vec[vec > 0],
    breaks = breaks,
    include.lowest = TRUE,
    labels = 1:(n-1)
  )
  return(cat)
}