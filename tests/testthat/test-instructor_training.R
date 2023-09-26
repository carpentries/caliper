test_that("rename_and_group_availability works with expected input", {
  df1 <- data.frame("Are.you.available.to.teach" = c("Yes", "No"))
  df2 <- data.frame("Are you available to teach" = c("Yes", "No"))
  df_out_1 <- rename_and_group_availability(df1)
  df_out_2 <- rename_and_group_availability(df2)
  expect_equal(names(df_out_1), "availability")
  expect_equal(names(df_out_2), "availability")
})

test_that("rename_and_group_availability throws error if no matching column exists",
          {
            df <- data.frame("unrelated_column" = c("Yes", "No"))
            expect_error(rename_and_group_availability(df),
                         "No matching column name found.")
          })

test_that("rename_and_group_availability fails if multiple matching columns",
          {
            df <- data.frame(
              "Are.you.available.to.teach" = c("Yes", "No"),
              "Are.you.available.to.teach.online" = c("Yes", "No")
            )

            expect_error(rename_and_group_availability(df),
                         "Error: Multiple matching columns found.")

          })

test_that("rename_and_group_availability leaves other columns unchanged", {
  df <- data.frame("Name" = c("Alice", "Bob"), "Are you available to teach" = c("Yes", "No"))
  df_out <- rename_and_group_availability(df)
  expect_true("Name" %in% names(df_out))
})

test_that("rename_and_group_availability accepts and returns data.frame", {
  df <- data.frame("Are you available to teach" = c("Yes", "No"))
  df_out <- rename_and_group_availability(df)
  expect_true(is.data.frame(df_out))
})

test_that("map_availability works with expected input", {
  df <- data.frame(
    "Are you available to teach" = c("Yes, I am available to teach online Instructor Training",
                                     "No, I can NOT teach online events next quarter.",
                                     "Maybe, but I am not able to schedule firmly at this time.")
  )
  result <- map_availability(df)
  expect_equal(result$availability, c("yes", "no", "maybe"))
})

test_that("map_availability renames the availability column", {
  df <- data.frame(
    "Are you available to teach" = c("Yes, I am available to teach online Instructor Training")
  )
  result <- map_availability(df)
  expect_true("availability" %in% names(result))
})

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
