# Global variables -----
.DATASET_DIR <- here::here("data-raw/nurses-stress/")


#' Read in nurses' stress data file
#'
#' @param file_path this is the oath to a data file
#' @param max_rows this is the number of rows shown in the output. Default 100
#'
#'
#' @returns outputs a dataframe/tibble
read <- function(file_path, max_rows = 100) {
  data <- file_path %>%
    readr::read_csv( # to explicitly tell R studio to use read_csv from readr package
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}


#' Read_all ".csv.gz" files in the stress/ folder into one data frame
#'
#' @param filename give the filename we want to read in
#' @param max_rows specify the number of rows it should load in
#'
#' @returns returns a table consisting of all files with the filename given as input

read_all <- function(filename, max_rows = 10) {
  files <- .DATASET_DIR %>%
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files %>%
    purrr::map(\(file) read(file, max_rows = max_rows)) %>%
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}

#' get_participant_id function to get participant id extracted
#'
#' @param data input the data that needs to have ID extracted
#'
#' @returns A data frame with a new column with ID extracted file_path_id removed
get_participant_id <- function(data){
  data_id <- data %>%
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        pattern = "(?<=/stress/)[:alnum:]{2}(?=/)"
      ),
      .before = file_path_id #this argument in mutate is used to add the new column before the "file_path_id" column
    ) %>%
    dplyr::select(-file_path_id)

  return(data_id)
}
