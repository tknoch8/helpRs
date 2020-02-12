#' @title turn a tibble or dataframe into a tsibble
#' @description create a 'time series tibble' from a tibble or dataframe
#' @usage make_tsibble(df, date_column, group_column,
#' period = c("day", "week", "month", "quarter", "year"))
#' @param df the tibble or dataframe from which to make a tsibble
#' @param date_column the column containing a date for each observation
#' @param group_column the column by which to group the data for each date
#' @param period the lubridate::floor_date period to floor the observation dates to
#' @return a tsibble with the specified time interval as the Key
#' @importFrom dplyr mutate group_by summarize ungroup
#' @importFrom lubridate floor_date
#' @importFrom tsibble as_tsibble fill_gaps
#' @examples
#' \dontrun{
#' nycflights13::weather %>%
#'   make_tsibble(df = .,
#'                date_column = time_hour,
#'                group_column = origin,
#'                period = "week")
#' }
#' @export
make_tsibble <- function(df, date_column, group_column,
                         period = c("day", "week", "month", "quarter", "year")) {

  if(!period %in% c("day", "week", "month", "quarter", "year"))
    stop("period must be one of: day, week, month, quarter, year")

  df %>%
    mutate(period_date = lubridate::floor_date({{date_column}}, unit = period)) %>%
    group_by({{group_column}}, period_date) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(period_date = as.Date(period_date)) %>%
    as_tsibble(index = period_date,
               key = {{group_column}},
               regular = TRUE,
               pivot_longer = TRUE) %>%
    fill_gaps(.full = TRUE,
              n = 0L)
}
