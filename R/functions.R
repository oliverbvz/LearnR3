#' Read in nurses' stress data file
#'
#' @param file_path this is the oath to a data file
#' @param max_rows this is the number of rows shown in the output. Default 100
#'
#'
#' @returns outputs a dataframe/tibble
read <- function(file_path, max_rows = 10) {
  data <- file_path %>%
    readr::read_csv( #to explicitly tell R studio to use read_csv from readr package
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}
