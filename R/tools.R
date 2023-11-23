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

#' Clean and Summarize "Select All That Apply" Survey Responses
#'
#' This function processes survey data where respondents could select multiple answers (e.g., "Select all that apply" questions).
#' It separates the responses, trims whitespace, and counts the occurrences of each unique response.
#'
#' @param dat A data frame containing the survey data.
#' @param col A character string specifying the name of the column with the "select all" responses.
#'
#' @importFrom dplyr mutate count
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim
#'
#' @return A data frame with each row representing a unique response from the specified column
#'         and its count in the survey.
#' @export
#'
#' @examples
#' # Example usage with a sample data frame
#' sample_data <- data.frame(responses = c("Option 1, Option 2", "Option 2, Option 3", "Option 1, Option 3, Option 2","Option 2","Option 3"))
#' cleaned_responses <- clean_select_all_responses(sample_data, "responses")
#' print(cleaned_responses)
clean_select_all_responses <- function(dat, col) {
  col_sym <- sym(col)
  response_table <- dat %>%
    separate_rows(!!col_sym, sep = ",") %>%
    mutate(!!col_sym := str_trim(!!col_sym)) %>%
    count(!!col_sym)
  return(response_table)
}

#' Load a List of R Libraries, Installing if Necessary
#'
#' This function iterates through a list of R package names, checks if they are installed,
#' and loads them into the R session. It installs any package that is not already installed.
#'
#' @param libraries A character vector of R package names to be loaded.
#'
#' @return None; the function is called for its side effects (loading libraries).
#' @export
#'
#' @examples
#' # Example usage to load dplyr and ggplot2
#' load_libraries(c("dplyr", "ggplot2"))
load_libraries <- function(libraries) {
  for (lib in libraries) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib)
      library(lib, character.only = TRUE)
    }
  }
}

#' Group Data by a Specified Column and Summarize Counts
#'
#' This function groups a data frame by the values in a specified column, then calculates
#' the count of records in each group. Optionally, it can convert these counts into percentages.
#'
#' @param dat A data frame to be analyzed.
#' @param col A character string specifying the column name by which to group the data.
#' @param convert_to_percent A logical value; if TRUE, convert counts to percentages.
#'                           Default is FALSE.
#'
#' @return A data frame with each group (from the specified column) and its count (and optionally, percentage).
#' @export
#'
#' @examples
#' # Example usage with a sample data frame
#' set.seed(1)  # for reproducibility
#' sample_data <- data.frame(category = sample(c("A", "B", "C"), 120, replace = TRUE, prob = c(0.3, 0.5, 0.2)))

#' summary_table <- group_by_summarise(sample_data, "category", convert_to_percent = TRUE)
#' print(summary_table)
group_by_summarise <- function(dat, col, convert_to_percent = FALSE){
  col_sym <- sym(col) #convert column name to symbol
  table <- dat %>%
    group_by(!!col_sym) %>% # unquote the symbol
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))

  # Optionally convert count to percent
  if (convert_to_percent) {
    total_count <- sum(table$count)

    table <- table %>%
      mutate(percent = (count / total_count) * 100)
  }

  return(table)

}
