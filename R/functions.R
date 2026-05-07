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


#' summarised_data a function to summarise data by datetime
#'
#' @param data a data frame
#'
#' @returns a data with new collumns of mean, sd, and median per minute

summarise_by_datetime <- function(data, unit) {
  summarised_data <- data %>%
    dplyr::mutate(
      collection_datetime = lubridate::round_date(collection_datetime,
                                                  unit = "minute"
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(
          mean = mean,
          sd = sd,
          median = median
        )
      ),
      .by = c(id, collection_datetime)
    )

  return(summarised_data)
}

' read_sensor_data a function to import sensor data from the nurses
#'
#' @param filename The filename for the data to be imported
#'
#' @returns a dataframe with summarised data

read_sensor_data <- function(filename, max_rows = 100, unit = "minute") {
  data <- read_all(filename, max_rows = max_rows) %>%
    get_participant_id() %>%
    summarise_by_datetime(unit = unit)
  return(data)
}

#' tidy_survey_dates to tidy the survey data dates
#'
#' @param data survey data
#'
#' @returns a tidied dataframe with tidied and correct dates
tidy_survey_dates <- function(data){
  tidied <- data %>%
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id =start_datetime,
      .before = start_time) %>%
    dplyr::select(-c(date, start_time, end_time, duration))

  return(tidied)
}

#' survey_to_long a function that formats data into long format
#'
#' @param data insert tidied survey datetime data
#'
#' @returns returns the survey data in a long format
survey_to_long <- function(data) {
  longer <- data %>%
    dplyr::select(id, datetime_id, start_datetime, end_datetime) %>%
    tidyr::pivot_longer(
      c(
        start_datetime,
        end_datetime
      ),
      names_to = NULL, # this removes the default column "name" that pivot_longer creates
      values_to = "collection_datetime"
    ) %>%
    dplyr::group_by(dplyr::pick(-collection_datetime)) %>%
    tidyr::complete(
      collection_datetime = seq(
        min(collection_datetime),
        max(collection_datetime),
        by = 60
      )
    ) %>%
    dplyr::ungroup()


  return(longer)
}
