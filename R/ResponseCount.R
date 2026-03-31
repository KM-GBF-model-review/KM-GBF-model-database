
# database <- LoadDatabaseInR(dataset_path = "input/Merged_Dataset_KMGBF_26Aug.xlsx")
# 
# response_columns <- .ColumnsWithAttributes(database)
# 
# response_columns
# 
# response <- response_columns[32]

ResponseCount <- function(database, response, radial = FALSE, shiny = FALSE){
  

  db_response <- database[,response]

  name <- attr(db_response, "name")
    
  n <- sum(!is.na(db_response))
  
  values <- attr(db_response, "value")
  
  data <- data.frame(values = values, frequency = NA)
  
  
  count <- suppressWarnings(table(unlist(as.numeric(
    stringr::str_split(db_response, pattern = ",", simplify = TRUE)
  ))))
  


  data[sort(as.numeric(names(count))),"frequency"] <- 
    as.numeric(count)[sort(as.numeric(names(count)))]
  
  data$values <- factor(data$values, levels = values)
  
 
  lim <- nrow(database)
  
  
  radar <- ggplot2::ggplot(data = data, ggplot2::aes(x = values, y = frequency, fill = frequency, label = frequency)) +
    # geom_text(label = "Draft", x =1, y = 1, size = 7, angle = 45, size.unit = "cm", colour = "grey", alpha = 0.1) +
    ggplot2::geom_col(alpha =0.95, position = ggplot2::position_dodge(2), colour = "grey30") + 
    ggplot2::geom_text(colour = "white", ggplot2::aes(y = frequency/2), size = ifelse(radial,5,10),fontface = "bold" ) +
    ggplot2::scale_fill_gradientn(colours = rev(viridis::magma(10)), limits = c(0,lim), name = "frequency") +
    ggplot2::scale_x_discrete(labels = stringr::str_wrap(data[,1], 30)) +
    ggplot2::ggtitle(label = glue::glue("{name}"),subtitle = glue::glue("N = {n}")) +
    #ggtitle(glue::glue("{paste(name)}\n N = {ifelse(qa, n_qa,64)}"))+ 
    ggplot2::ylab("Frequency") +
    ggplot2::ylim(c(0,lim)) +
     ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 25, face = "bold", angle = 0),
          axis.text.x = ggplot2::element_text(size = 20, face = "bold", angle = 70, hjust = 1, vjust= 1),
          axis.title.y = ggplot2::element_text(size = 20, face = "bold"),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = "white", color = "white"),
          panel.grid = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          title = ggplot2::element_text(face = "bold"), 
          legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA), # Transparent legend background
          legend.box.background = ggplot2::element_rect(fill = 'transparent', colour = NA), # Transparent legend panel
          legend.title.position = "top",
          legend.title = ggplot2::element_text(face = "bold"),
          legend.text = ggplot2::element_text(face = "bold"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = ggplot2::unit(2.2, "cm"),
          legend.key.height = ggplot2::unit(0.2, "cm"),
          plot.margin = ggplot2::unit(c(1,1,1,1), "cm"))
  

  if(shiny){
    radar <- radar +
      ggplot2::theme(legend.key.width = ggplot2::unit(10, "cm"),
                     legend.key.height = ggplot2::unit(0.5, "cm"),
                     legend.title = ggplot2::element_text(face = "bold", size = 20),
                     legend.text = ggplot2::element_text(face = "bold", size = 15),
                     title = ggplot2::element_text(face = "bold", size = 25), 
                     plot.subtitle =  ggplot2::element_text(size = 20))
  }
  
  
  
  if(radial){
    radar <- radar +
    ggplot2::coord_radial() +
      ggplot2::geom_hline(yintercept = seq(0,lim + 4,by = 5), colour = "lightgrey") +
      ggplot2::geom_vline(xintercept = c(1:(nrow(data))), colour = "lightgrey") +
      ggplot2::scale_x_discrete(labels = stringr::str_wrap(data[,1], 11)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, face = "bold", angle = 0),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0,0,0,0), "cm"))
  }
  
  
  return(radar)

}