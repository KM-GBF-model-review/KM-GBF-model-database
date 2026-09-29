ColouredBinaryTableDT <- function(database, responses, question = FALSE) {
  database <- database |>
    dplyr::filter(!(Name_of_the_model_with_acronym %in% c("EDv3", "ED2")))

  col_colours <- wesanderson::wes_palette(
    name = "AsteroidCity1",
    n = length(responses),
    type = "continuous"
  )

  response_lengths <- purrr::map_vec(responses, .f = function(x) {
    length(attr(database[, x], "value"))
  })

  response_names <- purrr::map_vec(responses, .f = function(x) {
    attr(database[, x], ifelse(question, "question", "name"))
  })

  tab_data <- purrr::map_dfc(responses, .f = function(response) {
    db_response <- database[, c("Name_of_the_model_with_acronym", response)]

    values <- attr(database[, response], "value")

    mat <- matrix(
      rep(0, nrow(db_response) * length(values)),
      nrow = nrow(db_response),
      dimnames = list(db_response[, "Name_of_the_model_with_acronym"], values)
    )

    for (i in 1:nrow(db_response)) {
      if (is.na(db_response[i, response])) {
        next()
      }

      mat[
        i,
        as.numeric(stringr::str_split_1(
          as.character(db_response[i, response]),
          pattern = ","
        ))
      ] <-
        1
    }

    return(as.data.frame(mat))
  })

  tab_data[, "Model name"] <- database[, "Name_of_the_model_with_acronym"]
  tab_data <- tab_data[, c(ncol(tab_data), 1:(ncol(tab_data) - 1))]

  tab_data[tab_data == 0] <- NA

  response_values <- purrr::map(responses, .f = function(x) {
    attr(database[, x], "value")
  }) |>
    Reduce(f = "c")

  # Build DT container with group (spanner) headers
  sketch <- htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          th(
            rowspan = 2,
            style = "font-style:normal;font-weight:bold;vertical-align:bottom;",
            "Model name"
          ), # Unspanned "Model" column
          Map(
            function(name, span, col) {
              th(
                colspan = span,
                style = "text-align:center;vertical-align:bottom;font-weight:bold;",
                name
              )
            },
            response_names,
            response_lengths,
            col_colours
          )
        ),
        tr(
          lapply(response_values, function(nm) {
            th(
              style = "font-style:italic;font-weight:normal;vertical-align:bottom;",
              nm
            )
          }) # Italic for all value headers
        )
      )
    )
  )

  # ----- END: HEADER/SPANNER CONTAINER -----

  # ----- BEGIN: ROW CALLBACK FOR COLORING -----

  rowCallback <- DT::JS(sprintf(
    "function(row, data, index) {
    var colors = ['%s'];
    for(var col=1; col<data.length; col++) {
      if(data[col] == 1 || data[col] == '1'){
        $('td:eq('+col+')', row).css({'background-color': colors[col-1], 'color': colors[col-1]});
      } else {
        $('td:eq('+col+')', row).css({'background-color': '#fff', 'color': '#fff'});
      }
    }
    $('td:eq(0)', row).css({'font-weight':'bold','text-align':'right','width':'150px'});
  }",
    paste(rep(col_colours, times = response_lengths), collapse = "','")
  ))

  # ----- END: ROW CALLBACK FOR COLORING -----

  # ----- BEGIN: DATATABLE RENDERING -----

  out <- DT::datatable(
    tab_data,
    container = sketch,
    escape = FALSE,
    rownames = FALSE,
    class = "compact",
    extensions = 'FixedColumns',
    options = list(
      dom = "t",
      ordering = FALSE,
      pageLength = nrow(tab_data),
      rowCallback = rowCallback,
      columnDefs = list(list(width = '150px', targets = 0)),
      scrollY = "1200px",
      scrollX = "1600px",
      fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 1)
    )
  )

  return(out)
}
