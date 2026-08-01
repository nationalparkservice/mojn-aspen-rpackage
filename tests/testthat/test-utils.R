


data <- loadAndWrangleMOJNAspen()

test_that("Test that loadAndWrangleMOJNAspen() works", {
  # Compare expected and actual lists in return object
  actual_list <- names(data)
  expected_list <- "data"
  expect_equal(actual_list, expected_list)

  # Compare expected and actual names of the aspen data frames
  actual_tbls <- names(data$data)
  expected_tbls <- c("SiteVisit", "Disturbances", "Observations", "Pests")
  expect_equal(actual_tbls, expected_tbls)

  # Check that the first object in the data list is a dataframe
  returnType <- data$data[[1]]
  expect_s3_class(returnType, "data.frame")
})
