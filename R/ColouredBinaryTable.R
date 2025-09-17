
ColouredBinaryTable <- function(database, responses, question = FALSE){
  
  
  col_colours <- wesanderson::wes_palette(name = "AsteroidCity1", n = length(responses), type = "discrete")
  
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

  
    tab_data <-  purrr::map_dfc(responses,.f = function(response){

  db_response <- database[,c("Name_of_the_model_with_acronym",
                             response)]
  
  
  values <- attr(database[,response], "value")
  
  mat <- matrix(rep(0, nrow(db_response)*length(values)),nrow = nrow(db_response),
                dimnames = list(db_response[,"Name_of_the_model_with_acronym"], values))
  

for(i in 1:nrow(db_response)){

  if(is.na(db_response[i,response])){
    next()
  }
    
  mat[i,as.numeric(stringr::str_split_1(db_response[i,response], pattern = ","))] <- 
    1
}
  
  return(as.data.frame(mat))

  
  })
  
    
  tab_data[,"Model"] <- database[,"Name_of_the_model_with_acronym"]
  tab_data <- tab_data[,c(ncol(tab_data),1:(ncol(tab_data)-1))]
  
  tab_data[tab_data == 0] <- NA
  
  
  
  bin_table <- gt::gt(tab_data) |>
    gt::tab_style(style = gt::cell_borders(sides = "all", color = "#000000", style = "solid", weight = gt::px(1)),
                  locations = gt::cells_body()) |>
    gt::data_color(
      columns = 1,
      palette = "grey50",na_color = "white"
    ) |>
    gt::cols_label(
      gt::starts_with("Model") ~ gt::md(glue::glue("**Model**")) 
    ) |>
    gt::tab_style(style = gt::cell_text(size = "large", weight = "bold", align = "right"),
                  locations = gt::cells_body(columns = 1,
                  ))



  
for(vec in response_values){
    
    bin_table <- bin_table |>
      gt::cols_label(
        gt::starts_with(vec) ~ gt::md(glue::glue("*{paste(vec)}*")) 
      )
}


k <- 2
  for(i in 1:length(response_lengths)){
    
    columns <- k:(k+(response_lengths[i] - 1))
    
    bin_table <- bin_table |>
      gt::tab_spanner(label = gt::md(glue::glue("**{response_names[i]}**")), columns = columns) |>
      gt::data_color(
        method = "numeric",columns = columns,
        palette = col_colours[i],na_color = "white"
      ) |>
      gt::sub_missing(missing_text = "") |>
      gt::tab_style(style = gt::cell_text(color = col_colours[i]),
                    locations = gt::cells_body(columns = columns,
                    ))
    
    k <- k+response_lengths[i]
  }
  
bin_table
  return(bin_table)
  
  
  
}
