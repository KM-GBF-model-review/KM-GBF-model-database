## Function to separate columns
separate_columns <- function(df, columns, delim) {
  for (col in columns) {
    df <- separate_longer_delim(df, col = all_of(col), delim = delim)
  }
  df <- df %>%
    mutate(across(everything(), str_trim))
  
  return(df)
}

### Create table with the number of publications covering of a specific variable 
tab_var_coverage <- function(data, var, total.pub = 1268) { 
  
  var1 <- ensym(var)
  
  x <- data %>% 
    group_by(paper_id, !!var1) %>%
    count(!!var1) %>%
    ungroup() %>%
    count(!!var1) %>%
    arrange(desc(n)) %>%
    mutate(prop = round(n/total.pub * 100,0))
  
  return(x)  
  
}


### Create table with the number of publications for one or multiple items of a specific variable 
tab_var_mult <- function(data, var, total.pub = 1268) { 
  
  var1 <- ensym(var)
  
  x <- data %>%
    group_by(paper_id, !!var1) %>%
    count(paper_id) %>%
    ungroup() %>%
    count(paper_id) %>%
    summarise(Multiple = sum(n > 1),
              Single = sum(n == 1)) %>%
    pivot_longer(c(Multiple, Single), values_to = "pubs") %>%
    mutate(prop = round(pubs/total.pub * 100,0))
  
  return(x)
  
}