
test_that("fetch_redash returns a data frame", {
  query_id <- 17
  result <- fetch_redash(query_id)
  expect_s3_class(result, "data.frame")
})

test_that("fetch_redash returns non-empty data frame", {
  query_id <- 17
  result <- fetch_redash(query_id)

  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

