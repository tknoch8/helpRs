#' @title get missing percent within a column of a tibble or a dataframe
#' @description get the percent of observations that are missing in a column of a tibble or dataframe.
#' Can be mapped or future_mapped over multiple columns in a dataframe
#' @usage get_missing_percent(data, missing_value, ...)
#' @param data is the dataframe or tibble to which the column of interest belongs
#' @param missing_value is the value to be counted as "missing", i.e "NA" or "", or c("NA", NA, "")
#' @param ... additional arguments passed
#' @return a character string of the missing percentage of miss_col
#' @importFrom scales percent
#' @examples
#' \dontrun{
#' require(tibble)
#'
#' col1 <- seq(1, 10, 1)
#' col2 <- c("NA", 5, 7, NA, 41, "NA", 6, 8, "NA", NA)
#' my_tibble <- tibble(col1 = col1, col2 = col2)
#' get_missing_percent(my_tibble$col1, missing_value = "NA")
#' }
#' @export
get_missing_percent <- function(data, missing_value = NA, ...) {

  char_data <- as.character(data)

  prop_missing <- length(char_data[char_data %in% c(missing_value)]) / length(char_data)

  percent(prop_missing)
}


#' @title print the date
#' @description print today's date like 2019_05_21
#' @usage make_date_underscored()
#' @return a character string of the date
#' @importFrom stringr str_replace_all
#' @examples
#' \dontrun{
#' make_date_underscored()
#' }
#' @export
make_date_underscored <- function() {
  my_date <- Sys.Date()
  my_date <- str_replace_all(my_date, "-", "_")
  my_date
}


#' @title print a timestamp
#' @description print a timestamp like "20190521_0800"
#' @usage make_time_stamp()
#' @return a character string of the timestamp
#' @importFrom stringr str_replace_all str_sub str_remove_all
#' @examples
#' \dontrun{
#' make_time_stamp()
#' }
#' @export
make_time_stamp <- function() {

  time_stamp <- as.character(Sys.time())
  time_stamp <- str_replace_all(time_stamp, "-", "")
  time_stamp <- str_replace_all(time_stamp, " ", "_")
  time_stamp <- str_sub(time_stamp, 1, -4)
  time_stamp <- str_remove_all(time_stamp, ":")

  as.character(time_stamp)
}


#' @title replace empty ("") values with "NA"
#' @description "NA" will be easier to see and deal with compared to empty
#'   values. Further testing is needed for this function
#' @usage df %>% empty_as_na()
#'
#'   # or
#'
#'   empty_as_na(df)
#' @param df the dataframe or tibble to be modified
#' @return df with empty values replaced with "NA"
#' @importFrom dplyr mutate_all if_else
#' @examples
#' \dontrun{
#' require(tibble)
#'
#' col1 <- seq(1, 10, 1)
#' col2 <- c("", 3, 4, "", 6, 7, 8, "", 10, "")
#' my_df <- tibble(col1 = col1, col2 = col2)
#' empty_as_na(df = my_df)
#' }
#' @export
empty_as_na <- function(df) {
  df %>%
    mutate_all(~as.character(.)) %>%
    mutate_all(~if_else(. == "", "NA", .))
}
