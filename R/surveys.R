

#' Calculate Net Promoter Score
#'
#' #'How to calculate net promoter score: https://monkeylear.com/blog/nps-analysis/
#'
#' @param dat
#' @param col
#'
#' @return
#' @export
#'

#'
#' @examples
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

#' Gather Questions into One Table to Graph Together
#'
#' @param dat
#' @param questions
#' @param levels
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
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
