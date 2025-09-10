
ModelInform <- function(database, responses, question = FALSE){
  
  response_values <- purrr::map(responses, .f = function(x){
    
    attr(database[,x], "value")
    
  }) |>
    Reduce(f = "c") |>
    unique()
  
  
  response_lengths <- purrr::map_vec(responses, .f = function(x){
    
    length(attr(database[,x], "value"))
    
  })
  
  
  response_names <- purrr::map_vec(responses, .f = function(x){
    
    attr(database[,x], ifelse(question,"question","name"))
    
  })
  
    data <-  purrr::map_dfc(responses,.f = function(response){

  db_response <- database[,c("Name_of_the_model_with_acronym",
                             response)]
  
  
  values <- attr(database[,response], "value")
  
  data <- matrix(rep(0, nrow(db_response)*length(values)),nrow = nrow(db_response),
                dimnames = list(db_response[,"Name_of_the_model_with_acronym"], values))
  

for(i in 1:nrow(db_response)){

  if(is.na(db_response[i,response])){
    next()
  }
    
  data[i,as.numeric(stringr::str_split_1(db_response[i,response], pattern = ","))] <- 
    1
}
  
  return(as.data.frame(data))

  
  })
  
    
  rownames(data) <- database[,"Name_of_the_model_with_acronym"]
  data[data == 0] <- NA
  
  
  bin_table <- gt::gt(data, rownames_to_stub = TRUE) |>
    gt::data_color(
      method = "numeric",
      palette = c("#E69736"),na_color = "white"
    ) |>
    gt::sub_missing(missing_text = "") |>
    gt::tab_style(style = gt::cell_text(color = "#E69736"),
                  locations = gt::cells_body(
                  )) |>
    gt::opt_stylize(style = 5) 
  
  
  
for(vec in response_values){
    
    bin_table <- bin_table |>
      gt::cols_label(
        gt::starts_with(vec) ~ gt::md(glue::glue("*{paste(vec)}*")) 
      )
}


k <- 2
  for(i in 1:length(response_lengths)){
    
    bin_table <- bin_table |>
      gt::tab_spanner(label = gt::md(glue::glue("**{response_names[i]}**")), columns = k:(k+(response_lengths[i] - 1)))
    
    k <- k+response_lengths[i]
  }
  

  
  return(bin_table)
  
  
  
}



data <- as.data.frame(data)
data[data == 0] <- NA


test <- gt::gt(data, rownames_to_stub = TRUE) |>
  gt::data_color(
    method = "numeric",
    palette = c("red"),na_color = "white"
  ) |>
  gt::sub_missing(missing_text = "") |>
  gt::tab_spanner(label = "test", 2:5)

test

