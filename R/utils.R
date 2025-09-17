.ColumnsWithAttributes <- function(database){
  
  cols <- sapply(database, function(col) !is.null(attributes(col)))
 
  return(names(which(cols)))
}


.AttributeValues <- function(database, attribute){
  
  attribute_vals <- sapply(database, function(col) attr(col,attribute)) |>
    Reduce(f = "c")
  
  return(attribute_vals)
}


.ColumnFromAttribute <- function(database, attribute, value){
  
  col_att <- .ColumnsWithAttributes(database = database)
  
  att_vals <- .AttributeValues(database, attribute)
  

 return(col_att[which(att_vals == value)] )
  
}


