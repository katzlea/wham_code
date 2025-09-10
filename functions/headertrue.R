header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}