find_all_paths <- function(graph, start, end, path = c()) {
  path <- c(path, start)
  
  # If we reached the end node, return the path
  if (start == end) {
    return(list(path))
  }
  
  paths <- list()
  for (neighbor in neighbors(graph, start, mode = "out")$name) {
    if (!(neighbor %in% path)) {  # Avoid cycles
      new_paths <- find_all_paths(graph, neighbor, end, path)
      paths <- c(paths, new_paths)
    }
  }
  
  return(paths)
}