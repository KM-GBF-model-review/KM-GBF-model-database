ColouredBinaryTable <- function(database, responses, question = FALSE) {
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

  tab_data[, "Model"] <- database[, "Name_of_the_model_with_acronym"]
  tab_data <- tab_data[, c(ncol(tab_data), 1:(ncol(tab_data) - 1))]

  tab_data[tab_data == 0] <- NA

  response_values <- purrr::map_dfr(seq_along(responses), .f = function(x) {
    data.frame(
      name = rep(response_names[x], response_lengths[x]),
      values = attr(database[, responses[x]], "value")
    )
  })

  test <- t(tab_data) |> as.data.frame()

  test <- test[-1, ]
  test <- cbind(response_values, test)

  test_table <- gt::gt(test, rowname_col = "values", groupname_col = "name") |>
    gt::tab_options(row_group.as_column = TRUE) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "#000000",
        style = "solid",
        weight = gt::px(1)
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::css(
        "transform" = "rotate(-90deg)",
        #"white-space" = "nowrap",
        #"font-size" = "10px",
        "padding-bottom" = "15px",
        "height" = "80px"
      ),
      locations = gt::cells_column_labels(everything())
    ) |>
    gt::cols_width(
      dplyr::everything() ~ gt::px(20)
    ) |>
    gt::cols_width(
      c(name, values) ~ gt::px(100)
    )

  gt::gtsave(test_table, "test_table.png")

  test_table

  bin_table <- gt::gt(tab_data) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "#000000",
        style = "solid",
        weight = gt::px(1)
      ),
      locations = gt::cells_body()
    ) |>
    gt::data_color(
      columns = 1,
      palette = "white",
      na_color = "white"
    ) |>
    gt::cols_label(
      gt::starts_with("Model") ~ gt::md(glue::glue("**Model**"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = "large", weight = "bold", align = "right"),
      locations = gt::cells_body(columns = 1, )
    )

  col_labs <- colnames(tab_data)[-1]

  for (i in 1:length(col_labs)) {
    bin_table <- bin_table |>
      gt::cols_label(
        !!col_labs[i] := gt::md(glue::glue("*{paste(response_values[i])}*"))
      )
  }

  k <- 2
  for (i in 1:length(response_lengths)) {
    columns <- k:(k + (response_lengths[i] - 1))

    bin_table <- bin_table |>
      gt::tab_spanner(
        label = gt::md(glue::glue("**{response_names[i]}**")),
        columns = columns
      ) |>
      gt::data_color(
        method = "numeric",
        columns = columns,
        palette = col_colours[i],
        na_color = "white"
      ) |>
      gt::sub_missing(missing_text = "") |>
      gt::tab_style(
        style = gt::cell_text(color = col_colours[i]),
        locations = gt::cells_body(columns = columns, )
      )

    k <- k + response_lengths[i]
  }

  bin_table <- bin_table |>
    # Set first column width to 150px
    gt::cols_width(
      "Model" ~ px(150) # Replace 'rowname' with your first column's name
    )

  bin_table

  return(bin_table)
}
