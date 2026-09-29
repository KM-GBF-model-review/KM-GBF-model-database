ColouredBinaryTableFlextable <- function(
  database,
  responses,
  question = FALSE,
  save_xlsx = FALSE,
  save_out = c()
) {
  database <- database |>
    dplyr::filter(!(Name_of_the_model_with_acronym %in% c("EDv3", "ED2")))

  col_colours <- wesanderson::wes_palette(
    name = "AsteroidCity1",
    n = length(responses),
    type = ifelse(length(responses) > 5, "continuous", "discrete")
  )

  set.seed(123)
  col_colours <- sample(
    col_colours,
    size = length(col_colours),
    replace = FALSE
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

  flex_tab <- tab_data[, -1] |> t() |> as.data.frame()

  rownames(flex_tab) <- NULL

  response_values <- purrr::map_dfr(seq_along(responses), .f = function(x) {
    data.frame(
      name = rep(response_names[x], response_lengths[x]),
      value = attr(database[, responses[x]], "value")
    )
  })

  flex_tab <- cbind(response_values, flex_tab)

  # Build flextable
  ft <- flextable::flextable(flex_tab) |>
    flextable::merge_v(j = 1) |>
    flextable::rotate(
      j = 3:ncol(flex_tab),
      rotation = "btlr",
      part = "header",
      align = "right"
    ) |>
    flextable::width(j = 3:ncol(flex_tab), width = 0.1) |>
    flextable::set_header_labels(
      values = c("", "", colnames(flex_tab)[3:ncol(flex_tab)])
    ) |>
    flextable::height(height = 1.4, part = "header") |>
    flextable::border(
      border = officer::fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    flextable::bold(j = 1, bold = TRUE, part = "body") |>
    flextable::bold(j = 3:ncol(flex_tab), bold = TRUE, part = "header") |>
    flextable::fontsize(j = 3:ncol(flex_tab), size = 12, part = "header")

  # Color cells with value 1 (background and text)

  for (col in 3:ncol(flex_tab)) {
    k <- 1
    for (grp in unique(flex_tab$name)) {
      idx <- which(flex_tab[[col]] == 1 & flex_tab$name == grp)
      if (length(idx) > 0) {
        ft <- flextable::bg(
          ft,
          i = idx,
          j = col,
          bg = col_colours[k],
          part = "body"
        )
        ft <- flextable::color(
          ft,
          i = idx,
          j = col,
          color = col_colours[k],
          part = "body"
        )
      }
      k <- k + 1
    }
  }

  if (save_xlsx) {
    file.out <- save_out
    # Initialize an openxlsx2 workbook
    wb <- openxlsx2::wb_workbook()$add_worksheet("My Table")

    # Add the flextable to the workbook (offsets to a specific cell, e.g., "B2")
    wb <- flexlsx::wb_add_flextable(wb, "My Table", ft, dims = "B2")

    # Overwrite numeric columns with actual numeric values so they are not
    # stored as strings (flexlsx coerces everything to character)
    # dims = "B2": header is row 3 (1 offset + 1 header), body starts row 4
    # Columns B = col 1 (name), C = col 2 (value), D onwards = numeric cols
    body_start_row <- 3 # row 1 is blank offset, row 2 is col names header, row 3 is blank?, row 4 is body
    numeric_cols <- seq(3, ncol(flex_tab))
    # Excel column index: dims starts at B (col 2), so flex_tab col 1 = sheet col B (2)
    for (j in numeric_cols) {
      sheet_col <- openxlsx2::int2col(j + 1) # +1 because table starts at col B
      col_data <- flex_tab[[j]]
      for (i in seq_along(col_data)) {
        if (!is.na(col_data[i])) {
          sheet_row <- body_start_row + i - 1
          wb <- openxlsx2::wb_add_data(
            wb,
            sheet = "My Table",
            x = as.numeric(col_data[i]),
            dims = paste0(sheet_col, sheet_row),
            col_names = FALSE
          )
        }
      }
    }

    # Save the file
    wb$save(file.out)
  } else {
    return(ft)
  }
}
