dir_to_df <- function(files,my_path) {
  setwd(my_path)
  files <- list.files(full.names=TRUE,recursive = TRUE, include.dirs = FALSE)
  files <- gsub("^./","",files)
  df <- data.frame(column= unlist(files)) %>% 
    separate(column, sep="/", into=c("site","image"))
}

