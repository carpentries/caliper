#' Simplify Trainer Availability Information
#'
#' This function takes a data frame and simplifies the availability information for trainers.
#' It maps verbose availability statements to simplified categories ("yes", "maybe", "no").
#' The function searches for a column with the name matching a pattern related to 'availability to teach' and performs the mapping in that column.
#'
#' @param dat Dataframe; The data frame containing trainer availability information. Comes from Trainer Quarterly Availability Form.
#'
#' @return Dataframe; The modified data frame with a simplified 'availability' column.
#' Throws an error if no column or multiple columns match the pattern for availability.
#'
#' @importFrom dplyr mutate case_when
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assume 'trainer_data' is a data frame with an appropriate availability column
#'   simplified_data <- map_availability(trainer_data)
#'   print(simplified_data)
#' }

map_availability <- function(dat) {
  maybe_vector <-
    c(
      "Maybe, but I am not able to schedule firmly at this time.  (Be sure to click \"Submit\" on the next page.)",
      "Maybe, but I am not able to schedule firmly at this time."
    )
  no_vector <- c(
    "No, I can NOT teach online events next quarter.",
    "No, I can NOT teach online events during that time.  (Be sure to click \"Submit\" on the next page.)"
  )

  yes_vector <- c(
    "Yes, I am available to teach online Instructor Training",
    "Yes, I am available to teach online Instructor Training and/or Bonus Modules"
  )
  # The regex pattern matches both spaces and dots
  matching_cols <-
    grepl("^Are you available to teach",
          names(dat),
          ignore.case = TRUE)

  if (sum(matching_cols) > 1) {
    stop("Error: Multiple matching columns found.")
  }
  if (sum(matching_cols) == 0) {
    stop("No matching column name found.")
  }
  col_index <-
    which(matching_cols)

  names(dat)[col_index] <- "availability"

  dat <- dat %>%
    mutate(
      availability = case_when(
        availability %in% maybe_vector ~ "maybe",
        availability %in% no_vector ~ "no",
        availability %in% yes_vector ~ "yes",
        TRUE ~ availability
      )
    )
  return(dat)
}

#' Simplify and Group Timezone Information
#'
#' This function takes a data frame and maps verbose timezone information to
#' simplified timezone groups. It identifies the column containing the timezone information
#' based on a pattern and renames it to 'timezone'.
#'
#' @param df Dataframe; The data frame containing timezone information. Comes from Trainer Quarterly Availability Form.
#'
#' @return Dataframe; The data frame with a renamed 'timezone' column and an additional 'TZgroup'
#' column containing simplified timezone labels.
#' Throws an error if no matching column or multiple matching columns are found.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr rename mutate
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assume 'data_with_timezones' is a data frame with an appropriate timezone column
#'   grouped_timezones <- map_timezones(data_with_timezones)
#'   print(grouped_timezones)
#' }
#'
map_timezones <- function(df) {
  # The regex pattern matches both spaces and dots
  matching_cols <-
    grepl(
      "^What time zone are you located in",
      names(df),
      ignore.case = TRUE
    )
  if (sum(matching_cols) > 1) {
    stop("Error: Multiple matching columns found.")
  }
  if (sum(matching_cols) == 0) {
    stop("No matching column name found.")
  }
  col_index <- which(matching_cols)
  df <- df %>%
    rename(timezone = names(df)[col_index])

  df <- df %>% mutate(
    TZgroup = case_when(
      stringr::str_detect(timezone, "\\bUTC[-]11\\b") ~ "TZ1",
      stringr::str_detect(timezone, "\\bUTC[-]12\\b") ~ "TZ1",
      stringr::str_detect(timezone, "\\bUTC[-]10\\b") ~ "TZ1",
      stringr::str_detect(timezone, "\\bUTC[-]9\\b") ~ "TZ1",
      stringr::str_detect(timezone, "\\bUTC[-]8\\b") ~ "TZ2",
      stringr::str_detect(timezone, "\\bUTC[-]7\\b") ~ "TZ2",
      stringr::str_detect(timezone, "\\bUTC[-]6\\b") ~ "TZ2",
      stringr::str_detect(timezone, "\\bUTC[-]5\\b") ~ "TZ2",
      stringr::str_detect(timezone, "\\bUTC[-]4\\b") ~ "TZ3",
      stringr::str_detect(timezone, "\\bUTC[-]3\\b") ~ "TZ3",
      stringr::str_detect(timezone, "\\bUTC[-]2[:]?30\\b") ~ "TZ3",
      stringr::str_detect(timezone, "\\bUTC[-]3\\b") ~ "TZ3",
      stringr::str_detect(timezone, "\\bUTC[-]1\\b") ~ "TZ3",
      stringr::str_detect(timezone, "\\bUTC[)]\\b") ~ "TZ4",
      stringr::str_detect(timezone, "\\bUTC[+]1\\b") ~ "TZ4",
      stringr::str_detect(timezone, "\\bUTC[+]2\\b") ~ "TZ4",
      stringr::str_detect(timezone, "\\bUTC[+]3\\b") ~ "TZ4",
      stringr::str_detect(timezone, "\\bUTC[+]4\\b") ~ "TZ5",
      stringr::str_detect(timezone, "\\bUTC[+]5\\b") ~ "TZ5",
      stringr::str_detect(timezone, "\\bUTC[+]6\\b") ~ "TZ5",
      stringr::str_detect(timezone, "\\bUTC[+]7\\b") ~ "TZ5",
      stringr::str_detect(timezone, "\\bUTC[+]8\\b") ~ "TZ6",
      stringr::str_detect(timezone, "\\bUTC[+]9\\b") ~ "TZ6",
      stringr::str_detect(timezone, "\\bUTC[+]10\\b") ~ "TZ6",
      stringr::str_detect(timezone, "\\bUTC[+]11\\b") ~ "TZ6",
      stringr::str_detect(timezone, "\\bUTC[+]12\\b") ~ "TZ6",
      TRUE ~ "TZother"
    )
  )

  return(df)
}

#' Create Scheduling Groups
#'
#'
#' @param df Dataframe; The data frame containing timezone information. Comes from Trainer Quarterly Availability Form.
#'
#' @return Dataframe; The data frame with an additional 'sched_group'
#' column.
#' Throws an error if no matching column or multiple matching columns are found.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr rename mutate
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assume 'data_with_timezones' is a data frame with an appropriate timezone column
#'   grouped_timezones <- map_timezones(data_with_timezones)
#'   print(grouped_timezones)
#' }
#'
map_scheduling_group <- function(df) {
  # The regex pattern matches both spaces and dots
  matching_cols <-
    grepl(
      "^What time zone are you located in",
      names(df),
      ignore.case = TRUE
    )
  if (sum(matching_cols) > 1) {
    stop("Error: Multiple matching columns found.")
  }
  if (sum(matching_cols) == 0) {
    stop("No matching column name found.")
  }
  col_index <- which(matching_cols)
  df <- df %>%
    rename(timezone = names(df)[col_index])

  df <- df %>%
    mutate(
      FD_ET = ifelse(
        str_detect(timezone, "UTC[-]7|UTC[-]6|UTC[-]5|UTC[-]4|UTC[-]3|UTC[-]2[:]?30"), "FD_ET", NA), #EST +/- 2, also includes Newfoundland timezone
      FD_PT = ifelse(
        str_detect(timezone, "UTC[-]10|UTC[-]9|UTC[-]8|UTC[-]7|UTC[-]6"), "FD_PT", NA), # PST +/- 2
      FD_CET = ifelse(
        str_detect(timezone, "UTC[-]1|UTC[)]|UTC[+]1|UTC[+]2"), "FD_CET", NA), # CEST +/- 2
      FD_AET = ifelse(
        str_detect(timezone, "UTC[+]12|UTC[+]11|UTC[+]10|UTC[+]9|UTC[+]8"), "FD_AET", NA), # AEST +/- 2
      HD_PM_PT = ifelse(
        str_detect(timezone, "UTC[+]10|UTC[+]11|UTC[+]12|UTC[-]12|UTC[-]11|UTC[-]10|UTC[-]9|UTC[-]8|UTC[-]7|UTC[-]6"), "HD_PM_PT", NA), # PST + 6 hours before and 2 hours after
      HD_PM_ET = ifelse(
        str_detect(timezone, "UTC[-]11|UTC[-]10|UTC[-]9|UTC[-]8|UTC[-]7|UTC[-]6|UTC[-]5|UTC[-]4|UTC[-]3"), "HD_PM_ET", NA),
      HD_PM_CET = ifelse(
        str_detect(timezone, "UTC[-]5|UTC[-]4|UTC[-]3|UTC[-]2[:]?30|UTC[-]2|UTC[-]1|UTC[ ]?0|UTC[+]1|UTC[+]2|UTC[+]3"), "HD_PM_CET", NA),
      HD_PM_AET = ifelse(
        str_detect(timezone, "UTC[+]4|UTC[+]5|UTC[+]6|UTC[+]7|UTC[+]8|UTC[+]9|UTC[+]10|UTC[+]11|UTC[+]12"), "HD_PM_AET", NA)
    )


  return(df)
}

#' Extract Quarter and Year Information from Data Frame Name
#'
#' This function extracts the quarter and year information from a given data frame name.
#' The name is expected to be in the format "Q(1-4)_(YYYY)" where (1-4) is the quarter and (YYYY) is the year.
#'
#' @param filename Dataframe; The data frame whose name you want to analyze.
#'
#' @return List; A list containing two elements: 'quarter' and 'year', both as character strings.
#'         The function will throw an error if the name does not match the expected format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create a data frame with a name that follows the format
#'   Q1_2034 <- data.frame()
#'   result <- extract_year_quarter(Q1_2034)
#'   print(result) # Should print list(quarter = "Q1", year = "2034")
#' }
#'
extract_year_quarter <- function(filename) {
  # Capture the name of the data frame as a string
  df_name <- deparse(substitute(filename))

  # Use a regular expression to extract the quarter and year
  matches <-
    regmatches(df_name, regexec("^(q[1-4]|Q[1-4])_([0-9]{4})$", df_name))[[1]]

  if (length(matches) != 3) {
    stop("Data frame name does not match the expected format.")
  }

  list(quarter = matches[2], year = matches[3])
}


#' Summarize Trainer Availability by Quarter
#'
#' This function takes a data frame containing trainer availability data, processes it,
#' and returns a summarized version. The summary is grouped by year, quarter, and availability status.
#' Currently, the year and quarter are hard-coded as "2034" and "1", respectively.
#'
#' @param dat Dataframe; The original data frame containing trainer availability data.
#'            Must contain a column that can be mapped to 'availability' by the map_availability() function.
#'
#' @return Dataframe; A new data frame containing the number of trainers available,
#'         maybe available, and not available, grouped by year and quarter.
#'
#' @seealso \code{\link{map_availability}} for how availability statuses are mapped.
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'   # Assuming 'trainer_data' is a data frame containing availability info
#'   summarized_data <- summarise_data_by_qtr(trainer_data)
#'   print(summarized_data)
#' }
#'

#To do: Make this work with real quarter and year instead of 2034 01
summarise_data_by_qtr <- function(dat) {
  dat %>%
    map_availability() %>%
    mutate(year = "2034",
           quarter = "1") %>%
    group_by(year, quarter, availability) %>%
    summarise(Number = n())
}

#' Summarize Trainer Availability by Timezone Group and Quarter
#'
#' This function takes a data frame with trainer availability and timezone information,
#' applies mapping and summarization operations, and returns a new data frame.
#' The summarized data is grouped by year, quarter, timezone group, and availability status.
#' Note that the year and quarter are currently hard-coded as "2034" and "1", respectively.
#'
#' @param dat Dataframe; Contains original trainer data including availability and timezones.
#'            Must have columns that can be mapped to 'availability' and 'timezones' by the
#'            map_availability() and map_timezones() functions, respectively.
#'
#' @return Dataframe; A new data frame containing the number of trainers in each timezone group,
#'         broken down by availability status, and grouped by year and quarter.
#'
#' @seealso \code{\link{map_availability}}, \code{\link{map_timezones}}
#'         for how availability and timezones are mapped.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming 'trainer_data' is a data frame with availability and timezone info
#'   summarized_data <- summarise_data_by_tz(trainer_data)
#'   print(summarized_data)
#' }
#'

summarise_data_by_tz <- function(dat) {
  dat %>%
    mutate(year = "2034",
           quarter = "1") %>%
    map_availability() %>%
    map_timezones() %>%
    group_by(year, quarter, TZgroup, availability) %>%
    summarise(Number = sum(n()))
}

#' Preprocess Trainee Data
#'
#' This function preprocesses the trainee data by converting columns to dates and calculating the number of days
#' from the training date to the current date (`days_from_training`) and from the training date to the badging date (`days_to_badge`).
#'
#' @param trainee_data data.frame
#'   A data frame containing the raw trainee data. Expected to have columns named "training" and "instr_badge",
#'   which represent the training and instruction badging dates, respectively.
#'
#' @return data.frame
#'   A preprocessed data frame with added columns:
#'   - `days_from_training`: Number of days from training date to today's date.
#'   - `days_to_badge`: Number of days from training date to badging date.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_trainee_data <- data.frame(
#'     training = c("2023-01-01", "2023-01-10"),
#'     instr_badge = c("2023-02-01", NA)
#'   )
#'   preprocessed_data <- preprocess_trainee_data(raw_trainee_data)
#'   print(preprocessed_data)
#' }
preprocess_trainee_data <- function(trainee_data) {
  preprocessed_data <- trainee_data %>%
    mutate(
      training = as.Date(training),
      instr_badge = as.Date(instr_badge),
      days_from_training = Sys.Date() - training,
      days_to_badge = instr_badge - training
    )

  return(preprocessed_data)
}

#' Count of Trainees Who Have Passed a Given Number of Days Since Training
#'
#' This function calculates the number of trainees who are past a specified number of days since their training.

#' @param trainee_data data.frame
#'   A raw data frame containing trainee data with columns `training` and `instr_badge` as date strings.
#'

#' @param days_threshold integer
#'   The number of days past the training date to consider. Defaults to 90 days.
#'
#' @return integer
#'   Returns the count of trainees who are past the specified number of days since their training.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_trainee_data <- data.frame(
#'     training = c("2023-01-01", "2023-01-10"),
#'     instr_badge = c("2023-02-01", NA)
#'   )
#'   count <- trainees_n_days(raw_trainee_data, 90)
#'   print(count)
#' }
#'
trainees_n_days <- function(trainee_data, days_threshold = 90) {
  # Preprocess the data first
  preprocessed_data <- preprocess_trainee_data(trainee_data)

  trainee_days <- preprocessed_data %>%
    filter(days_from_training >= days_threshold) %>%
    nrow()

  return(trainee_days)
}


#' Count of Trainees Badged Within a Given Number of Days Since Training
#'
#' This function calculates the number of trainees who have been badged within a specified number of days since their training.
#'
#'@param trainee_data data.frame
#'   A raw data frame containing trainee data with columns `training` and `instr_badge` as date strings.
#'
#' @param days_threshold integer
#'   The maximum number of days between training and badging to consider. Defaults to 90 days.
#'
#' @return integer
#'   Returns the count of trainees who have been badged within the specified number of days since their training.
#'
#' @export
#'
#' @examples
#'\dontrun{
#'   raw_trainee_data <- data.frame(
#'     training = c("2023-01-01", "2023-01-10"),
#'     instr_badge = c("2023-02-01", NA)
#'   )
#'   count <- badged_n_days(raw_trainee_data, 90)
#'   print(count)
#' }
badged_n_days <- function(trainee_data, days_threshold = 90) {
  # Preprocess the data first
  preprocessed_data <- preprocess_trainee_data(trainee_data)

  badged_days <- preprocessed_data %>%
    filter(
      !is.na(days_to_badge),
      days_to_badge <= days_threshold,
      days_from_training >= days_threshold
    ) %>%
    nrow()

  return(badged_days)
}

#' Calculate Checkout Rate
#'
#' This function calculates the checkout rate based on the number of trainees who earned a badge
#' within a specified number of days since their training.
#'
#' @param trainee_data data.frame
#'   A raw data frame containing trainee data. Should have columns `training` and `instr_badge` as date strings.
#'
#' @param days_threshold integer
#'   The time frame for which you would like to calculate the checkout rate, in days. Default is 90 days.
#'
#' @return numeric
#'   Returns the checkout rate as a percentage.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_data <- data.frame(...)
#'   checkout_rate <- calculate_checkout_rate(raw_data, 90)
#' }
calculate_checkout_rate <-
  function(trainee_data, days_threshold = 90) {
    trainee_days <- trainees_n_days(trainee_data, days_threshold)
    if (trainee_days == 0) {
      stop("trainee_days cannot be zero. Exiting function.")
    }
    badged_days <- badged_n_days(trainee_data, days_threshold)
    checkout_rate <- calculate_rate(badged_days, trainee_days)

    return(checkout_rate)
  }

#' Calculate Re-engagement Rate
#'
#' This function calculates the re-engagement rate based on the number of trainees who earned a badge
#' within a specified number of days since their training, typically over a longer period such as 6 months.
#'
#' @param trainee_data data.frame
#'   A raw data frame containing trainee data. Should have columns `training` and `instr_badge` as date strings.
#'
#' @param days_threshold integer
#'   The time frame for which you would like to calculate the re-engagement rate, in days. Default is 180 days.
#'
#' @return numeric
#'   Returns the re-engagement rate as a percentage.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_data <- data.frame(...)
#'   reengagement_rate <- calculate_reengagement_rate(raw_data, 180)
#' }
calculate_reengagement_rate <-
  function(trainee_data, days_threshold = 180) {
    trainee_days <- trainees_n_days(trainee_data, days_threshold)
    if (trainee_days == 0) {
      stop("trainee_days cannot be zero. Exiting function.")
    }
    badged_days <- badged_n_days(trainee_data, days_threshold)
    checkout_rate <- calculate_rate(badged_days, trainee_days)

    return(checkout_rate)
  }

#' Calculate Number of Trainees Who Have Started Checkout
#'
#' This function calculates the number of trainees who have started the checkout process within a specified number of days since their training.
#'
#' @param trainee_progress data.frame
#'   A data frame containing checkout steps for each trainee, like 'get_involved', 'teaching_demo', and 'welcome'.
#' @param days_threshold integer
#'   The number of days since training to consider. Default is 90 days.
#'
#' @return integer
#'   Returns the number of trainees who have started the checkout process.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_data <- data.frame(...)
#'   n_started_checkout <- calculate_checkout_started(raw_data, 90)
#' }
calculate_checkout_started <-
  function(trainee_progress, days_threshold = 90) {
    n_started_checkout <- trainee_progress %>%
      mutate(
        training = as.Date(training),
        instr_badge = as.Date(instr_badge),
        days_to_badge = instr_badge - training,
        days_from_training = Sys.Date() - training
      ) %>%
      filter(
        days_from_training >= days_threshold &
          (get_involved != "" | teaching_demo != ""
           |
             welcome != "")
      ) %>%
      nrow()

    return(n_started_checkout)
  }

#' Calculate Number of Trainees Who Started but Haven't Finished Checkout
#'
#' This function calculates the number of trainees who started but haven't completed the checkout process within a specified number of days since their training.
#'
#' @param trainee_progress data.frame
#'   A data frame containing progress indicators for each trainee.
#' @param days_threshold integer
#'   The number of days since training to consider. Default is 90 days.
#'
#' @return integer
#'   Returns the number of trainees who have started but not finished the checkout process.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_data <- data.frame(...)
#'   n_not_finished_checkout <- calculate_checkout_not_finished(raw_data, 90)
#' }
calculate_checkout_not_finished <-
  function(trainee_progress, days_threshold = 90) {
    n_not_finished_checkout <- trainee_progress %>%
      mutate(
        training = as.Date(training),
        instr_badge = as.Date(instr_badge),
        days_from_training = Sys.Date() - training
      ) %>%
      filter(
        days_from_training >= days_threshold &
          (get_involved != "" |
             teaching_demo != "" | welcome != "")
        & is.na(instr_badge)
      ) %>%
      nrow()

    return(n_not_finished_checkout)

  }

#' Calculate Instructor Checkout Dropout Rate
#'
#' This function calculates the dropout rate among trainees who have started the checkout process within a specified number of days since their training.
#'
#' @param trainee_progress data.frame
#'   A data frame containing progress indicators for each trainee.
#' @param days_threshold integer
#'   The number of days since training to consider. Default is 90 days.
#'
#' @return numeric
#'   Returns the dropout rate as a percentage.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   raw_data <- data.frame(...)
#'   dropout_rate <- calculate_dropout_rate(raw_data, 90)
#' }
calculate_dropout_rate <-
  function(trainee_progress, days_threshold = 90) {
    if (nrow(trainee_progress) == 0 ||
        all(
          trainee_progress$get_involved == "" &
          trainee_progress$teaching_demo == "" &
          trainee_progress$welcome == ""
        )) {
      return(NA)
    }
    n_started <- calculate_checkout_started(trainee_progress)
    n_not_finished <-
      calculate_checkout_not_finished(trainee_progress)
    dropout_rate <- calculate_rate(n_not_finished, n_started)

    return(dropout_rate)
  }

#' Calculate Average Time to Complete Checkout
#'
#' This function computes the average number of days trainees take to complete the checkout process, considering only trainees who have been involved for a specified number of days.
#'
#' @param trainee_progress data.frame
#'   A data frame containing columns for 'instr_badge' and 'training', which represent the checkout completion and training dates, respectively.
#' @param days_threshold integer
#'   The minimum number of days since training to include a trainee in the calculation. Default is 90 days.
#'
#' @return numeric
#'   Returns the average number of days to complete checkout, rounded to two decimal places.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   trainee_progress <- fetch_redash(388)
#'   avg_time <- calculate_avg_time_to_checkout(trainee_progress)
#' }
calculate_avg_time_to_checkout <-
  function(trainee_progress, days_threshold = 90) {
    trainee_progress <- trainee_progress %>%
      mutate(
        instr_badge = as.Date(instr_badge),
        training = as.Date(training),
        days_to_badge = instr_badge - training,
        days_from_training = Sys.Date() - training
      ) %>%
      filter(days_from_training >= days_threshold)

    avg_time_to_checkout <-
      round(mean(trainee_progress$days_to_badge, na.rm = TRUE), 2)

    return(avg_time_to_checkout)

  }

#' Calculate Mean of Workshop Survey Item Before and After Checkout Changes
#'
#' This function calculates the mean value for a specified workshop survey item, segmented by the time before and after training re-changes went live.
#'
#' @param data data.frame
#'   A data frame containing the survey responses. Must include a column for 'submitted_at' and the item to be analyzed.
#' @param item string
#'   The column name of the survey item for which the mean should be calculated.
#'
#' @return data.frame
#'   Returns a data frame with the mean value of the item before and after training re-changes, rounded to two decimal places.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   mock_data <- data.frame(submitted_at = as.Date(c("2023-08-15", "2023-08-13")), instructors_clear_answers = c(4, 5))
#'   mean_data <- calculate_wksurvey_item_mean(mock_data, "instructors_clear_answers")
#' }
calculate_wksurvey_item_mean <- function(data, item) {
  existing_cols <- intersect(
    names(data),
    c(
      'instructors_clear_answers',
      'instructors_enthusiastic',
      'instructors_comfortable_interaction',
      'instructors_knowledgeable',
      'recommendation_score'
    )
  )

  data <- data %>% mutate_at(existing_cols, as.numeric) %>%
    mutate(train_re_changes = as.factor(ifelse(
      submitted_at < as.Date("2023-08-14"), #go live date
      "before", "after"
    ))) %>%
    filter(!is.na(.data[[item]]) & !is.na(train_re_changes)) %>%
    group_by(train_re_changes) %>%
    summarise(Mean = round(mean(.data[[item]], na.rm = TRUE), 2)) %>%
    arrange(desc(train_re_changes))
  return(data)
}

#' Number of Trainings Trainers Have Taught
#'
#' Calculates the total number of trainings each trainer has taught, grouped by their active status.
#'
#' @param dat data.frame
#'   A data frame containing columns 'personal', 'family', 'active_status', and 'total_trainings'.
#'
#' @return data.frame
#'   Returns a data frame with columns 'personal', 'family', 'active_status', and 'total_trainings', ordered by 'active_status' and 'total_trainings'.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(personal = c("Alice", "Bob"), family = c("Smith", "Jones"), active_status = c("Active", "Inactive"), total_trainings = c(5, 2))
#'   result <- count_trainings(df)
#' }

count_trainings <- function(dat) {
  num_trainings <- dat %>%
    select(personal, family, active_status, total_trainings) %>%
    group_by(active_status) %>%
    arrange(active_status, desc(total_trainings))
  return(num_trainings)
}


#' Count of Trainings Conducted by Trainers by Year
#'
#' Calculates the number of trainings conducted by each trainer, grouped by year.
#'
#' @param dat data.frame
#'   A data frame containing columns 'family', 'personal', and 'start' (start date of the training).
#'
#' @return data.frame
#'   Returns a data frame grouped by 'family', 'personal', and 'year', with the number of trainings in that year.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(family = c("Smith", "Jones"), personal = c("Alice", "Bob"), start = c("2021-05-01", "2022-04-01"))
#'   result <- count_trainings_yr(df)
#' }
count_trainings_yr <- function(dat) {
  activity_by_year <- dat %>%
    mutate(year = lubridate::year(as.Date(start, format = "%Y-%m-%d"))) %>%
    group_by(family, personal, year) %>%
    summarise(Number = n())
  return(activity_by_year)
}

#' Count of Trainings by Relative Year from Start of Training Career
#'
#' Calculates the number of trainings conducted by each trainer, grouped by their career-relative year.
#'
#' @param dat data.frame
#'   A data frame containing columns 'family', 'personal', and 'start' (start date of the training).
#'
#' @return data.frame
#'   Returns a data frame grouped by 'year' and 'relative_year', summarizing the number of trainings in that relative year.
#'
#' @export
#' @importFrom lubridate year
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(family = c("Smith", "Jones"), personal = c("Alice", "Bob"), start = c("2021-05-01", "2022-04-01"))
#'   result <- count_trainings_rel_yr(df)
#' }
count_trainings_rel_yr <- function(dat) {
  trainer_activity <- dat %>%
    mutate(year = lubridate::year(as.Date(start)))

  # Calculate Year of first event for each trainer
  trainer_start_yr <- trainer_activity %>%
    group_by(family, personal) %>%
    summarise(start_year = min(year))

  # Merge start year into trainer_activity

  trainer_activity <-
    inner_join(trainer_activity,
               trainer_start_yr,
               by = c("family", "personal"))

  # Calculate relative year

  trainer_activity <- trainer_activity %>%
    mutate(relative_year = year - start_year)

  activity_by_relative_year <- trainer_activity %>%
    group_by(year, relative_year) %>%
    summarise(Number = n())
  return(activity_by_relative_year)
}
