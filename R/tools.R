#' Calculate a rate
#'
#' @param xsmaller
#' @param ylarger
#'
#' @return a percentage
#' @export
#'
#' @examples
#' calculate_rate(50, 100)
calculate_rate <- function(xsmaller, ylarger) {
  paste0(round(xsmaller / ylarger * 100, 2), "%")
}

#' Set Time Frame
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
set_time_frame <-  function(dat, col, start, end) {
  df <- dat %>%
    filter(col > as.Date(start, "%Y-%m-%d") &
             col < as.Date(end, "%Y-%m-%d"))

  return(df)
}
