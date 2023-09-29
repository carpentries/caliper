#' Calculate the Rate as a Percentage
#'
#' This function takes two numerical values as input, with the smaller number (`xsmaller`) and the larger number (`ylarger`).
#' It returns the rate as a percentage, calculated by the formula: (xsmaller / ylarger) * 100.
#'
#'
#' @param xsmaller Numeric; The smaller value you want to compare. Must be non-negative.
#' @param ylarger Numeric; The larger value to which you want to compare `xsmaller`. Must be greater than `xsmaller`.
#'
#' @return A string representing the rate as a percentage, rounded to two decimal places.
#' @export
#'
#' @examples
#' calculate_rate(50, 100) # Returns '50%'
#' calculate_rate(30, 120) # Returns '25%'
#' calculate_rate(0, 100)  # Returns '0%'
calculate_rate <- function(xsmaller, ylarger) {
  paste0(round(xsmaller / ylarger * 100, 2), "%")
}

#' Filter Data Frame by a Date Range
#'
#' This function filters a data frame based on a specific date range within a specified column.
#' The function utilizes `dplyr` to filter rows where the date in the specified column falls
#' between the start and end dates, exclusive.
#'
#' @param dat Dataframe; The data frame you wish to filter.
#' @param col Character; The column name containing date information to filter by.
#' @param start Date; The start date for the date range in 'YYYY-MM-DD' format.
#' @param end  Date; The end date for the date range in 'YYYY-MM-DD' format.Date; The end date for the date range in 'YYYY-MM-DD' format.Date; The end date for the date range in 'YYYY-MM-DD' format.
#'
#' @return A data frame containing rows that meet the date range criteria.A data frame containing rows that meet the date range criteria.
#' @export
#'@importFrom dplyr filter
#'@importFrom rlang sym
#'
#' @examples
#' \dontrun{
#'   trainee_progress <- fetch_redash(388)
#'   result <- set_time_frame(dat = trainee_progress, col = "training", start = "2021-01-01", end = "2021-12-31")
#'   head(result)
#' }
set_time_frame <-  function(dat, col, start, end) {
  col_sym <- sym(col)
  df <- dat %>%
    filter(!!col_sym > as.Date(start, "%Y-%m-%d") &
             !!col_sym < as.Date(end, "%Y-%m-%d"))

  return(df)
}
