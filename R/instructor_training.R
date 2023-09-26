#' Rename and Group Trainer Availability Data
#'
#' @param dat a dataframe
#'
#' @return
#' @export
#' @importFrom googlesheets4 read_sheet
#'
#' @examples
#' sample_data <- googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI")
#'
#' result <- rename_and_group_availability(sample_data)
#'
#' str(result)

rename_and_group_availability <- function(df) {
  # The regex pattern matches both spaces and dots
  matching_cols <- grepl("available[\\.\\s]to[\\.\\s]teach", names(df), ignore.case = TRUE)

  if(sum(matching_cols) > 1) {
    stop("Error: Multiple matching columns found.")
  }
  if (sum(matching_cols) == 0) {
    stop("No matching column name found.")
  }
  col_index <-
    which(matching_cols)

  names(df)[col_index] <- "availability"
  return(df)
}


#' Map Trainer Availability
#'
#' @param dat a dataframe
#'
#' @return data frame with availability column simplified
#' @export
#' @import dplyr
#' @importFrom googlesheets4 read_sheet
#'
#' @examples
#' sample_data <- googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI")
#'
#' result <- map_availability(sample_data)
#' str(result)
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

  dat <- dat %>%
    rename_and_group_availability() %>%
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

#' Map Timezones
#'
#' @param dat a dataframe
#'
#' @return dataframe with timezones simplified
#' @export
#' @importFrom stringr str_detect
#' @import dplyr
#'
#' @examples
#' sample_data_google <- googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI")
#'
#' result_1 <-
#' map_timezones(sample_data_google)
#' print(result_1)
#'
#' sample_data_csv <- read.csv("data/trainer_availability_sample_data.csv")
#' result_2 <- map_timezones(sample_data_csv)
#' print(result_2)
map_timezones <- function(df) {
  # The regex pattern matches both spaces and dots
  matching_cols <- grepl("What[\\.\\s]time[\\.\\s]zone[\\.\\s]are[\\.\\s]you[\\.\\s]located[\\.\\s]in", names(df), ignore.case = TRUE)
  if(sum(matching_cols) > 1) {
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
      stringr::str_detect(timezone, "\\bUTC[ ]?0\\b") ~ "TZ4",
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

#' Extract Quarter and Year from Filename
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
#' Q1_2034 <- data.frame()
#' result <- extract_year_quarter(Q1_2034)
#' print(result)
extract_year_quarter <- function(df) {
  # Capture the name of the dataframe as a string
  df_name <- deparse(substitute(df))

  # Use a regular expression to extract the quarter and year
  matches <-
    regmatches(df_name, regexec("^(q[1-4]|Q[1-4])_([0-9]{4})$", df_name))[[1]]

  if (length(matches) != 3) {
    stop("Data frame name does not match the expected format.")
  }

  list(quarter = matches[2], year = matches[3])
}



#' Summarise Trainer Availability Data by Quarter
#'
#' @param dat
#'
#' @return
#' @export
#' @import dplyr
#' @examples
#'q1_2034 <- as.data.frame(googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI"))
#'
#'result <- summarise_data_by_qtr(q1_2034)
#'
#' print(result)
#'
#' To do: Make this work with real quarter and year instead of 2034 01
summarise_data_by_qtr <- function(dat) {
  dat %>%
    map_availability() %>%
    mutate(year = "2034",
           quarter = "1") %>%
    group_by(year, quarter, availability) %>%
    summarise(Number = n())
}

#' Summarise Trainer Availability Data by Timezone Group
#'
#' @param dat data frame
#'
#' @return
#' @export
#'
#' @examples
#' q1_2034 <- as.data.frame(googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI"))
#'
#'result <- summarise_data_by_tz(q1_2034)
#'
#' print(result)
summarise_data_by_tz <- function(dat) {
  dat %>%
    mutate(year = "2034",
           quarter = "1") %>%
    map_availability() %>%
    map_timezones() %>%
    group_by(year, quarter, TZgroup, availability) %>%
    summarise(Number = sum(n()))
}

#' Process Trainer Availability Data
#'
#' @param dat data frame
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(dat) {
  dat <- rename_and_group_availability(dat)
  dat <- map_timezones(dat)
  dat <- map_availability(dat)

  dat %>% summarise_data_by_qtr()
  dat %>% summarise_data_by_tz()
}

#' Trainee Days
#'
#' @param trainee_data
#' @param days_threshold
#'
#' @return
#' @export
#'
#' @examples
trainees_n_days <- function(trainee_data, days_threshold = 90) {
  trainee_days <- trainee_data %>%
    mutate(
      training = as.Date(training),
      instr_badge = as.Date(instr_badge),
      days_from_training = Sys.Date() - training
    ) %>%
    filter(days_from_training >= days_threshold) %>%
    nrow()

  return(trainee_days)
}


#' Badged Days
#'
#' @param trainee_data
#' @param days_threshold
#'
#' @return
#' @export
#'
#' @examples
badged_n_days <- function(trainee_data, days_threshold = 90) {
  badged_days <- trainee_data %>%
    mutate(
      training = as.Date(training),
      instr_badge = as.Date(instr_badge),
      days_to_badge = instr_badge - training,
      days_from_training = Sys.Date() - training
    ) %>%
    filter(
      !is.na(instr_badge),
      days_to_badge <= days_threshold,
      days_from_training >= days_threshold
    ) %>%
    nrow()

  return(badged_days)
}

#' Calculate checkout rate
#'
#' @param trainee_data
#' @param days_threshold the number of days you would like to review, default = 90 days
#'
#' @return percentage
#' @export
#'
#' @examples
calculate_checkout_rate <-
  function(trainee_data, days_threshold = 90) {
    trainee_days <- trainees_n_days(trainee_data, days_threshold)
    badged_days <- badged_n_days(trainee_data, days_threshold)
    checkout_rate <- calculate_rate(badged_days, trainee_days)

    return(checkout_rate)
  }

#' Calculate Checkout Started
#'
#' @param trainee_progress
#' @param days_threshold
#'
#' @return number of people who have started checkout
#' @export
#'
#' @examples
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

#
#' Calculate checkout not finished
#'
#' @param trainee_progress
#' @param days_threshold
#'
#' @return number of people who have started but not finished checkout
#' @export
#'
#' @examples
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

#' Instructor Checkout Dropout Rate
#'
#' @param trainee_progress
#' @param days_threshold
#'
#' @return percentage
#' @export
#'
#' @examples
calculate_dropout_rate <-
  function(trainee_progress, days_threshold = 90) {
    n_started <- calculate_checkout_started(trainee_progress)
    n_not_finished <-
      calculate_checkout_not_finished(trainee_progress)
    dropout_rate <- calculate_rate(n_not_finished, n_started)

    return(dropout_rate)
  }

#' Calculate average time to checkout
#'
#' @param trainee_progress data frame
#' @param days_threshold number of days since training, default = 90
#'
#' @return average number of days
#' @export
#'
#' @examples
#' trainee_progress <- fetch_redash(388)
#' (calculate_avg_time_to_checkout(trainee_progress))
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

#' Calculate Workshop Survey Item Mean
#'
#' @param data dataframe
#' @param item column name
#'
#' @return
#' @export
#'
#' @examples
calculate_wksurvey_item_mean <- function(data, item) {
  go_live_date <- as.Date("2023-08-14")
  data <- data %>% mutate_at(
    c(
      'instructors_clear_answers',
      'instructors_enthusiastic',
      'instructors_comfortable_interaction',
      'instructors_knowledgeable',
      'recommendation_score'
    ),
    as.numeric
  ) %>%
    mutate(train_re_changes = as.factor(ifelse(
      submitted_at < go_live_date, "before", "after"
    ))) %>%
    filter(!is.na(!!sym(item)) & !is.na(train_re_changes)) %>%
    group_by(train_re_changes) %>%
    summarise(Mean = round(mean(!!sym(item), na.rm = TRUE), 2))

  return(data)
}
