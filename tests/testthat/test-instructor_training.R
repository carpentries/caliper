# Test if map_availability works with expected input and renames the column
test_that("map_availability works with expected input and renames the column", {
  df <- data.frame(
    "Are you available to teach" = c("Yes, I am available to teach online Instructor Training",
                                     "No, I can NOT teach online events next quarter.",
                                     "Maybe, but I am not able to schedule firmly at this time.")
  )
  result <- map_availability(df)
  expect_equal(result$availability, c("yes", "no", "maybe"))
  expect_true("availability" %in% names(result))
})

# Test if map_availability throws error if no matching column exists
test_that("map_availability throws error if no matching column exists", {
  df <- data.frame("unrelated_column" = c("Yes", "No"))
  expect_error(map_availability(df), "No matching column name found.")
})

# Test if map_availability fails if multiple matching columns
test_that("map_availability fails if multiple matching columns", {
  df <- data.frame(
    "Are.you.available.to.teach" = c("Yes", "No"),
    "Are.you.available.to.teach.online" = c("Yes", "No")
  )
  expect_error(map_availability(df), "Error: Multiple matching columns found.")
})

# Test if map_availability leaves other columns unchanged
test_that("map_availability leaves other columns unchanged", {
  df <- data.frame("Name" = c("Alice", "Bob"), "Are you available to teach" = c("Yes", "No"))
  df_out <- map_availability(df)
  expect_true("Name" %in% names(df_out))
})

# Test if map_availability maps unknown values unchanged
test_that("map_availability maps unknown values unchanged", {
  df <- data.frame(
    "Are you available to teach" = c("Unknown value")
  )
  result <- map_availability(df)
  expect_equal(result$availability, "Unknown value")
})


test_that("map_timezones works with expected input", {
  df <- data.frame(
    "What time zone are you located in?" = c("UTC+1", "UTC-5", "UTC+10")
  )
  result <- map_timezones(df)
  expect_equal(result$TZgroup, c("TZ4", "TZ2", "TZ6"))
})

test_that("map_timezones renames the timezone column", {
  df <- data.frame(
    "What time zone are you located in?" = c("UTC+1")
  )
  result <- map_timezones(df)
  expect_true("timezone" %in% names(result))
})

test_that("map_timezones handles unknown timezones", {
  df <- data.frame(
    "What time zone are you located in?" = c("Mars Standard Time")
  )
  result <- map_timezones(df)
  expect_equal(result$TZgroup, "TZother")
})

# To do: This is currently hard coded as a sample year because the function does not work to extract the year
test_that("extract_year_quarter works as expected", {
  # Prepare a mock dataframe
  Q1_2023 <- data.frame()

  # Test valid dataframe name
  result <- extract_year_quarter(Q1_2023)
  expect_equal(result$quarter, "Q1")
  expect_equal(result$year, "2023")

  # Prepare another mock dataframe with lowercase q
  q1_2023 <- data.frame()
  result <- extract_year_quarter(q1_2023)
  expect_equal(result$quarter, "q1")
  expect_equal(result$year, "2023")

  # Test invalid dataframe name: this should throw an error
  invalid_df <- data.frame()
  expect_error(extract_year_quarter(invalid_df), "Data frame name does not match the expected format.")

  # Test invalid year format: this should throw an error
  Q1_20 <- data.frame()
  expect_error(extract_year_quarter(Q1_20), "Data frame name does not match the expected format.")

  # Test invalid quarter format: this should throw an error
  Q5_2023 <- data.frame()
  expect_error(extract_year_quarter(Q5_2023), "Data frame name does not match the expected format.")
})

# To do: Fix year and quarter
test_that("summarise_data_by_qtr works as expected", {

  # Step 1: Mock data frame
  mock_data <- data.frame(
    "What time zone are you located in?" = c("UTC+1", "UTC-5", "UTC+10"),
    "Are you available to teach" = c("yes", "no", "maybe")
  )

  # Step 2: Expected result
  expected_result <- tibble(
    year = c("2034", "2034", "2034"),
    quarter = c("1", "1", "1"),
    availability = c("yes", "no", "maybe"),
    Number = c(1, 1, 1)
  )

  # Step 3: Running the test
  actual_result <- summarise_data_by_qtr(mock_data) %>%  ungroup()

  # Step 4: Sort before checking equality
  actual_result <- actual_result %>% arrange(year, quarter, availability)
  expected_result <- expected_result %>% arrange(year, quarter, availability)

  # Check if the output matches the expected result
  expect_equal(expected_result, actual_result)
})

# To do: Fix year and quarter
test_that("summarise_data_by_tz works as expected", {

  # Mock data frame
  mock_data <- data.frame(
    "What time zone are you located in?" = c("UTC+1", "UTC-5", "UTC+10"),
    "Are you available to teach" = c("yes", "no", "maybe")
  )

  # Expected result
  expected_result <- tibble(
    year = c("2034", "2034", "2034"),
    quarter = c("1", "1", "1"),
    TZgroup = c("TZ4", "TZ2", "TZ6"),
    availability = c("yes", "no", "maybe"),
    Number = c(1, 1, 1)
  )

  # Running the test
  actual_result <- summarise_data_by_tz(mock_data) %>% ungroup()

  # Sort before checking equality
  actual_result <- actual_result %>% arrange(year, quarter, TZgroup, availability)
  expected_result <- expected_result %>% arrange(year, quarter, TZgroup, availability)

  # Check if the output matches the expected result
  expect_equal(expected_result, actual_result)
})

# Create mock data for the tests
mock_instr_data <- data.frame(
  training = c("2021-01-01", "2022-01-01", "2022-06-30"),
  instr_badge = c("2021-02-01", "2022-02-01", "2022-07-30")
)

# Test 1: Check trainees_n_days returns correct number of rows
test_that("trainees_n_days returns correct number of trainees who meet the day threshold", {
  result <- trainees_n_days(mock_instr_data)
  expect_equal(result, 3) # All three trainees have more than 90 days since their training
})

# Test 2: Check if trainees_n_days returns 0 when no data meets the condition
test_that("trainees_n_days returns 0 when no trainee meets the day threshold", {
  result <- trainees_n_days(mock_instr_data, days_threshold = 3000)
  expect_equal(result, 0)
})

# Test 3: Check trainees_n_days with different days_threshold
test_that("trainees_n_days works with different days_threshold", {
  result <- trainees_n_days(mock_instr_data, days_threshold = 180)
  expect_equal(result, 3) # All three should meet this condition
})

# Create mock data for the tests
mock_badge_data <- data.frame(
  training = c("2021-01-01", "2022-01-01", "2022-06-30"),
  instr_badge = c("2021-01-10", "2022-01-10", NA)
)

# Test 1: Check badged_n_days returns correct number of rows when conditions are met
test_that("badged_n_days returns correct number of trainees when conditions are met", {
  result <- badged_n_days(mock_badge_data)
  expect_equal(result, 2) # Two trainees have badges and more than 90 days since their training
})

# Test 2: Check if badged_n_days returns 0 when no data meets the conditions
test_that("badged_n_days returns 0 when no trainee meets the conditions", {
  result <- badged_n_days(mock_badge_data, days_threshold = 3000)
  expect_equal(result, 0)
})

# Test 3: Check badged_n_days with different days_threshold
test_that("badged_n_days works with different days_threshold", {
  result <- badged_n_days(mock_badge_data, days_threshold = 10)
  expect_equal(result, 2) # Two trainees got their badges within 10 days and have more than 10 days since their training
})

# Mock data
mock_checkout_data <- data.frame(
  training = c("2022-01-01", "2022-01-10", "2022-05-01"),
  instr_badge = c("2022-02-01", "2022-02-10", NA)
)

# Test 1: Check calculate_checkout_rate returns correct rate when both functions return non-zero values
test_that("calculate_checkout_rate returns correct rate for non-zero values", {
  result <- calculate_checkout_rate(mock_checkout_data)
  # Assuming calculate_rate function simply divides badged_days by trainee_days
  # Here it should be 2/3 = 0.6667
  expect_equal(result, "66.67%")
})

# Test 2: calculate_checkout_rate returns 0 when no trainees have badges
test_that("calculate_checkout_rate returns 0 when no badges", {
  mock_data_no_badges <- data.frame(
    training = c("2022-01-01", "2022-01-10", "2022-05-01"),
    instr_badge = c(NA, NA, NA)
  )
  result <- calculate_checkout_rate(mock_data_no_badges)
  expect_equal(result, "0%")
})

# Test 3: calculate_checkout_rate works with different days_threshold
test_that("calculate_checkout_rate works with different days_threshold", {
  result <- calculate_checkout_rate(mock_checkout_data, days_threshold = 30)
  # Assuming in last 30 days the rate would be something different
  expect_equal(result, "0%") # Replace 'some_value' with what you expect
})

# Test 4: calculate_checkout_rate returns NA or throws an error when trainee_days is zero
test_that("calculate_checkout_rate returns NA or throws error when trainee_days is zero", {
  mock_data_empty <- data.frame(training = character(0), instr_badge = character(0))
  expect_error(calculate_checkout_rate(mock_data_empty), "trainee_days cannot be zero. Exiting function.")
})

# Test when trainee_progress is empty
test_that("calculate_checkout_started returns 0 when trainee_progress is empty", {
  mock_data_empty <- data.frame(
    training = character(0),
    instr_badge = character(0),
    get_involved = character(0),
    teaching_demo = character(0),
    welcome = character(0)
  )
  result <- calculate_checkout_started(mock_data_empty)
  expect_equal(result, 0)
})

# Test when days_from_training is below the threshold
test_that("calculate_checkout_started filters out records below days_threshold", {
  mock_data <- data.frame(
    training = Sys.Date() - c(30, 80, 100),
    instr_badge = NA,
    get_involved = c("", "", "Yes"),
    teaching_demo = c("", "", ""),
    welcome = c("", "", "")
  )
  result <- calculate_checkout_started(mock_data, days_threshold = 90)
  expect_equal(result, 1)
})

# Test when any of get_involved, teaching_demo, or welcome is non-empty
test_that("calculate_checkout_started counts only records with started checkouts", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = NA,
    get_involved = c("", "Yes", ""),
    teaching_demo = c("", "", "Yes"),
    welcome = c("Yes", "", "")
  )
  result <- calculate_checkout_started(mock_data, days_threshold = 90)
  expect_equal(result, 3)
})

# Test when all columns except days are empty
test_that("calculate_checkout_started returns 0 when no checkouts started", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = NA,
    get_involved = c("", "", ""),
    teaching_demo = c("", "", ""),
    welcome = c("", "", "")
  )
  result <- calculate_checkout_started(mock_data, days_threshold = 90)
  expect_equal(result, 0)
})


# Test when trainee_progress is empty
test_that("calculate_checkout_not_finished returns 0 when trainee_progress is empty", {
  mock_data_empty <- data.frame(
    training = character(0),
    instr_badge = character(0),
    get_involved = character(0),
    teaching_demo = character(0),
    welcome = character(0)
  )
  result <- calculate_checkout_not_finished(mock_data_empty)
  expect_equal(result, 0)
})

# Test when all trainees have instr_badge
test_that("calculate_checkout_not_finished returns 0 when all trainees have instr_badge", {
  mock_data <- data.frame(
    training = Sys.Date() - 100,
    instr_badge = Sys.Date() - 20,
    get_involved = "Yes",
    teaching_demo = "Yes",
    welcome = "Yes"
  )
  result <- calculate_checkout_not_finished(mock_data)
  expect_equal(result, 0)
})

# Test when days_from_training is below the threshold
test_that("calculate_checkout_not_finished filters out records below days_threshold", {
  mock_data <- data.frame(
    training = Sys.Date() - c(30, 50, 100),
    instr_badge = c(NA, NA, NA),
    get_involved = c("Yes", "Yes", "Yes"),
    teaching_demo = c("Yes", "Yes", "Yes"),
    welcome = c("Yes", "Yes", "Yes")
  )
  result <- calculate_checkout_not_finished(mock_data, days_threshold = 90)
  expect_equal(result, 1)
})

# Test when some trainees have started but not finished checkout
test_that("calculate_checkout_not_finished counts trainees who have started but not finished checkout", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = c(NA, NA, Sys.Date() - 20),
    get_involved = c("Yes", "Yes", "Yes"),
    teaching_demo = c("Yes", "Yes", "Yes"),
    welcome = c("Yes", "Yes", "Yes")
  )
  result <- calculate_checkout_not_finished(mock_data, days_threshold = 90)
  expect_equal(result, 2)
})

# Test when trainee_progress is empty
test_that("calculate_dropout_rate returns NA when trainee_progress is empty", {
  mock_data_empty <- data.frame(
    training = character(0),
    instr_badge = character(0),
    get_involved = character(0),
    teaching_demo = character(0),
    welcome = character(0)
  )
  result <- calculate_dropout_rate(mock_data_empty)
  expect_true(is.na(result))
})

# Test when no one has started the checkout
test_that("calculate_dropout_rate returns NA when no one has started", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = c(NA, NA, NA),
    get_involved = c("", "", ""),
    teaching_demo = c("", "", ""),
    welcome = c("", "", "")
  )
  result <- calculate_dropout_rate(mock_data)
  expect_true(is.na(result))
})

# Test when all who have started have also finished
test_that("Function returns 0 when all who started have finished", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = Sys.Date() - c(20, 30, 40),
    get_involved = c("Yes", "Yes", "Yes"),
    teaching_demo = c("Yes", "Yes", "Yes"),
    welcome = c("Yes", "Yes", "Yes")
  )
  result <- calculate_dropout_rate(mock_data)
  expect_equal(result, "0%")
})

# Test when some have started but not finished
test_that("Function calculates dropout rate correctly", {
  mock_data <- data.frame(
    training = Sys.Date() - c(100, 100, 100),
    instr_badge = c(NA, NA, Sys.Date() - 20),
    get_involved = c("Yes", "Yes", "Yes"),
    teaching_demo = c("Yes", "Yes", "Yes"),
    welcome = c("Yes", "Yes", "Yes")
  )
  result <- calculate_dropout_rate(mock_data)
  expect_equal(result, "66.67%") # Assuming calculate_rate function multiplies by 100
})

test_that("calculate_avg_time_to_checkout works as expected", {

  # Happy Path
  mock_data_1 <- data.frame(
    training = Sys.Date() - c(100, 101, 110),
    instr_badge = Sys.Date() - c(80, 85, 90)
  )
  # Create a difftime object
  expected_1 <- as.difftime(round(mean(c(20, 16, 20), na.rm = TRUE), 2), units = "days")

  result <- calculate_avg_time_to_checkout(mock_data_1)
  expect_equal(result, expected_1)

  # Edge case: Empty data
  mock_data_empty <- data.frame(
    training = character(0),
    instr_badge = character(0)
  )
  result <- calculate_avg_time_to_checkout(mock_data_empty)
  expect_true(is.na(result))

  # Different Date Ranges
  mock_data_2 <- data.frame(
    training = as.Date(c("2022-01-01", "2022-01-05", "2022-01-10")),
    instr_badge = as.Date(c("2022-01-21", "2022-01-15", "2022-01-20"))
  )
  expected_2 <-  as.difftime(round(mean(c(20, 10, 10), na.rm = TRUE), 2), units = "days")

  result <- calculate_avg_time_to_checkout(mock_data_2, days_threshold = 1)
  expect_equal(result, expected_2)

  # NA values in instr_badge
  mock_data_3 <- data.frame(
    training = Sys.Date() - c(100, 101, 110),
    instr_badge = c(NA, Sys.Date() - 85, Sys.Date() - 90)
  )
  expected_3 <- as.difftime(round(mean(c(NA, 16, 20), na.rm = TRUE), 2), units = "days")

  result <- calculate_avg_time_to_checkout(mock_data_3)
  expect_equal(result, expected_3)

  # Different Days Threshold
  mock_data_4 <- data.frame(
    training = Sys.Date() - c(50, 60, 70),
    instr_badge = Sys.Date() - c(30, 35, 40)
  )
  expected_4 <- as.difftime(round(mean(c(20, 25, 30), na.rm = TRUE), 2), units = "days")

  result <- calculate_avg_time_to_checkout(mock_data_4, days_threshold = 40)
  expect_equal(result, expected_4)

})

test_that("calculate_wksurvey_item_mean works as expected", {

  # Happy Path
  mock_data_happy <- data.frame(
    submitted_at = as.Date(c("2023-08-15", "2023-08-13")),
    instructors_clear_answers = c(4, 5)
  )
  result_happy <- calculate_wksurvey_item_mean(mock_data_happy, "instructors_clear_answers")
  expect_equal(result_happy[result_happy$train_re_changes == "after", ]$Mean, 4)
  expect_equal(result_happy[result_happy$train_re_changes == "before", ]$Mean, 5)

  # NA Values
  mock_data_na <- data.frame(
    submitted_at = as.Date(c("2023-08-15", "2023-08-13", "2023-08-15")),
    instructors_clear_answers = c(4, NA, 5)
  )
  result_na <- calculate_wksurvey_item_mean(mock_data_na, "instructors_clear_answers")
  expect_equal(result_na[result_na$train_re_changes == "after",]$Mean, 4.5) # After (NA should be ignored)

  # Date Handling
  mock_data_date <- data.frame(
    submitted_at = as.Date(c("2023-08-14", "2023-08-15")),
    instructors_clear_answers = c(4, 5)
  )
  result_date <- calculate_wksurvey_item_mean(mock_data_date, "instructors_clear_answers")
  expect_equal(result_date[result_date$train_re_changes == "after",]$Mean, 4.5) # After (both)

  # Item Doesn't Exist
  expect_error(calculate_wksurvey_item_mean(mock_data_date, "non_existent_item"))

  # Empty Data
  mock_data_empty <- data.frame(
    submitted_at = character(0),
    instructors_clear_answers = numeric(0)
  )
  result_empty <- calculate_wksurvey_item_mean(mock_data_empty, "instructors_clear_answers")
  expect_true(is.data.frame(result_empty) && nrow(result_empty) == 0)
})



