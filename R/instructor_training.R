#' Rename and Group Trainer Availability Data
#'
#' @param dat
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

rename_and_group_availability <- function(dat) {
  col_index <- which(startsWith(names(dat), "Are you available to teach"))
  if (length(col_index) == 0) {
    stop("No matching column name found.")
  }
  names(dat)[col_index] <- "availability"
  return(dat)
}


#' Map Trainer Availability
#'
#' @param dat
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
  maybe_vector <- c("Maybe, but I am not able to schedule firmly at this time.  (Be sure to click \"Submit\" on the next page.)",
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
    mutate(availability = case_when(
      availability %in% maybe_vector ~ "maybe",
      availability %in% no_vector ~ "no",
      availability %in% yes_vector ~ "yes",
      TRUE ~ availability
    ))
  return(dat)
}

#' Map Timezones
#'
#' @param dat
#'
#' @return dataframe with timezones simplified
#' @export
#' @importFrom stringr str_detect
#' @import dplyr
#'
#' @examples
#' sample_data <- googlesheets4::read_sheet("1epfbcCrR2WsaaDo_rNNvX8GrSJK0Px2m9ph6Oj6sEhI")
#'
#' result <-
#' map_timezones(sample_data)
#' print(result)
map_timezones <- function(dat) {
  dat <- dat %>%
    map_availability() %>%
    rename(timezone = "What time zone are you located in?")

  dat <- dat %>% mutate(
    TZgroup = case_when(
      stringr::str_detect(timezone, "UTC[-]12") ~ "TZ1",
      stringr::str_detect(timezone, "UTC[-]11") ~ "TZ1",
      stringr::str_detect(timezone, "UTC[-]10") ~ "TZ1",
      stringr::str_detect(timezone, "UTC[-]9") ~ "TZ1",
      stringr::str_detect(timezone, "UTC[-]8") ~ "TZ2",
      stringr::str_detect(timezone, "UTC[-]7") ~ "TZ2",
      stringr::str_detect(timezone, "UTC[-]6") ~ "TZ2",
      stringr::str_detect(timezone, "UTC[-]5") ~ "TZ2",
      stringr::str_detect(timezone, "UTC[-]4") ~ "TZ3",
      stringr::str_detect(timezone, "UTC[-]3") ~ "TZ3",
      stringr::str_detect(timezone, "UTC[-]2[:]?30") ~ "TZ3",
      stringr::str_detect(timezone, "UTC[-]3") ~ "TZ3",
      stringr::str_detect(timezone, "UTC[-]1") ~ "TZ3",
      stringr::str_detect(timezone, "UTC[ ]?0") ~ "TZ4",
      stringr::str_detect(timezone, "UTC[+]1") ~ "TZ4",
      stringr::str_detect(timezone, "UTC[+]2") ~ "TZ4",
      stringr::str_detect(timezone, "UTC[+]3") ~ "TZ4",
      stringr::str_detect(timezone, "UTC[+]4") ~ "TZ5",
      stringr::str_detect(timezone, "UTC[+]5") ~ "TZ5",
      stringr::str_detect(timezone, "UTC[+]6") ~ "TZ5",
      stringr::str_detect(timezone, "UTC[+]7") ~ "TZ5",
      stringr::str_detect(timezone, "UTC[+]8") ~ "TZ6",
      stringr::str_detect(timezone, "UTC[+]9") ~ "TZ6",
      stringr::str_detect(timezone, "UTC[+]10") ~ "TZ6",
      stringr::str_detect(timezone, "UTC[+]11") ~ "TZ6",
      stringr::str_detect(timezone, "UTC[+]12") ~ "TZ6",
      TRUE ~ "TZother"
    )
  )

  return(dat)
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
  matches <- regmatches(df_name, regexec("^(q[1-4]|Q[1-4])_([0-9]{4})$", df_name))[[1]]

  if(length(matches) != 3) {
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
#' @param dat
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
    map_timezones() %>%
    group_by(year, quarter, TZgroup, availability) %>%
    summarise(Number = sum(n()))
}

#' Process Trainer Availability Data
#'
#' @param dat
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

# Function to Generate Plot
#' Generate Plots
#'
#' @param dat
#' @param x
#' @param y
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
generate_plot <- function(dat, x, y, fill = NULL) {
  ggplot(dat, aes_string(x = x, y = y, fill = fill)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = after_stat(y)), stat = "identity", vjust = -0.3, position = position_dodge(width = 0.9)) +
    facet_grid(year ~ quarter) +
    theme_carpentries()
}
