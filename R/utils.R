.ColumnsWithAttributes <- function(database){
  
  cols <- sapply(database, function(col) !is.null(attributes(col)))
 
  return(names(which(cols)))
}


.AttributeValues <- function(database, atrribute){
  
  attribute_vals <- sapply(database, function(col){
    
    att <- attr(col, attribute)
    
    if(is.null(att)){
      att = NA
    }
    
    return(att)
    
  })
  
  return(attribute_vals)
}


.ColumnFromAttribute <- function(database, attribute, value){
  
  att_vals <- .AttributeValues(database, attribute)
  
 return(which(att_vals == value))
  
}


