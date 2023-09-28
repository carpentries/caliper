#' Calculate a rate
#'
#' @param xsmaller the smaller number
#' @param ylarger the larger number
#'
#' @return a percentage
#' @export
#'
#' @examples
#' calculate_rate(50, 100)
calculate_rate <- function(xsmaller, ylarger) {
  paste0(round(xsmaller / ylarger * 100, 2), "%")
}

#' Filter Data Frame by start and end date
#'
#' @param dat a dataframe
#' @param col a column in the dataframe
#' @param start the date you want to start the time frame
#' @param end  the date you want to end the time frame
#'
#' @return the dataframe filtered by the time range
#' @export
#'
#' @examples
#' trainee_progress <- fetch_redash(388)
#' result <- set_time_frame(dat = trainee_progress, col = "training", start = "2021-01-01", end = "2021-12-31")
#' head(result)
set_time_frame <-  function(dat, col, start, end) {
  col_sym <- sym(col)
  df <- dat %>%
    filter(!!col_sym > as.Date(start, "%Y-%m-%d") &
             !!col_sym < as.Date(end, "%Y-%m-%d"))

  return(df)
}
