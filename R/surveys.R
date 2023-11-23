

#' Calculate Net Promoter Score (NPS)
#'
#' This function calculates the Net Promoter Score from survey data based on respondents' ratings.
#' NPS is a measure of customer loyalty and is calculated using the formula:
#' Percentage of Promoters - Percentage of Detractors.
#' For more information on NPS, visit: https://monkeylearn.com/blog/nps-analysis/
#'
#' @param dat A data frame containing survey data.
#' @param col A character string specifying the name of the column containing NPS ratings (expected to be numeric).
#'
#' @return A data frame summarizing the number and percentage of respondents in each category:
#'         Promoters, Passives, and Detractors.
#' @export
#'
#' @examples
#' # Example usage with a sample data frame
#' sample_data <- data.frame(rating = sample(0:10, 100, replace = TRUE))
#' nps_result <- calculate_nps(sample_data, "rating")
#' print(nps_result)
calculate_nps <- function(dat, col) {
  # Ensure col is a character vector of length 1
  if (!is.character(col) || length(col) != 1) {
    stop("Error: 'col' should be a single column name as character.")
  }

  # Check in the column exists in the data frame
  if (!col %in% names(dat)) {
    stop("Error: Specified column does not exist in the data frame.")
  }

  # Check if hte dat in the column is numeric
  if (!is.numeric(dat[[col]])) {
    stop("Error: Data in the specified column is not numeric.")
  }
  # Calculate net promoter score
  table <- dat %>%
    mutate(Category = case_when(
      .[[col]] <= 6 ~ 'Detractor',
      .[[col]] %in% 7:8 ~ 'Passive',
      .[[col]] >= 9 ~ 'Promoter',
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Category)) %>%
    group_by(Category) %>%
    summarise(Number = n(), .groups = 'drop') %>%
    mutate(Percent = (Number / sum(Number)))

}

#' Gather Questions into One Table for Graphing
#'
#' This function transforms survey data by converting selected question columns into a long format
#' suitable for comparison and graphing. It facilitates analysis across multiple questions.
#'
#' @param dat A data frame containing survey data.
#' @param questions A character vector of column names in 'dat' representing the questions to be summarized.
#' @param levels A vector of the levels (possible responses) for the questions.
#' @param labels A vector of labels corresponding to the 'levels' vector for re-labeling responses.
#'
#' @importFrom dplyr select mutate group_by summarize
#' @importFrom tidyr gather
#'
#' @return A data frame in long format, with each row representing a response to a question,
#'         along with the count of each response type.
#' @export
#'
#' @examples
#' # Example usage with a sample data frame
#' sample_data <- data.frame(Q1 = sample(1:5, 100, replace = TRUE), Q2 = sample(1:5, 100, replace = TRUE))
#' questions <- c("Q1", "Q2")
#' levels <- 1:5
#' labels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
#' summarised_data <- gather_and_summarise(sample_data, questions, levels, labels)
#' print(summarised_data)
gather_and_summarise <- function(dat, questions, levels, labels){
  # Validation Checks
  if (!is.data.frame(dat)) {
    stop("Error: 'dat' should be a data frame.")
  }

  if (!all(questions %in% names(dat))) {
    stop("Error: Some questions in 'questions' are not column names in 'dat'.")
  }

  if (length(levels) != length(labels)) {
    stop("Error: The length of 'levels' and 'labels' must be the same.")
  }

  # Convert the selected columns into a long format
  tryCatch({
    table <- dat %>%
      select(all_of(questions)) %>%
      gather(key = "Question", value = "Answer") %>%
      mutate(Answer = factor(Answer, levels = levels, labels = labels)) %>%
      group_by(Question, Answer) %>%
      summarize(Number = n())

    return(table)
  }, error = function(e) {
    stop("Error in processing data: ", e$message)
  })
}
