.ColumnsWithAttributes <- function(database){
  
  cols <- sapply(database, function(col) !is.null(attributes(col)))
 
  return(names(which(cols)))
}
