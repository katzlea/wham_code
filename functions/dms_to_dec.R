dm_to_dec <- function(item, type) {
  if (type == "dm") {
    dgr <- substr(item, 1, 2)
    mn <- substr(item, 4, 9)
    dir <- substr(item, 11, 11)
    tot = as.numeric(dgr) + as.numeric(mn)/60
    if (grepl(dir,"S") || grepl(dir,"E") == "TRUE") {
      tot = -tot
    }
    return(tot)
  }
  if (type == "dms") {
    dgr <- substr(item, 1, 2)
    mn <- substr(item, 4, 5)
    sec <- substr(item, 7, 8)
    dir <- substr(item, 10, 10)
    tot = as.numeric(dgr) + as.numeric(mn)/60 + as.numeric(sec)/3600
    if (grepl(dir,"S") || grepl(dir,"E") == "TRUE") {
      tot = -tot
    }
    return(tot)
  }
}