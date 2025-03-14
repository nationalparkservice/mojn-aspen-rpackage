

# Things to test in fetchAndWrangleAspen - only function in utils rn
  # check that $data and $meta data exists
  # check that there are the correct number of tables in data and metadata
  # check that each data table has the correct number of columns and that the columns have the right names
  # check that the return type from function is correct

data <- loadAndWrangleMOJNAspen()

test_that("Test that loadAndWrangleMOJNAspen() works", {
  # Compare expected and actual names in return object
  actual_names <- names(data)
  expected_names <- c("data", "metadata")
  expect_equal(actual_names, expected_names)

  # Compare expected and actual names of the aspen data frames
  actual_cols <- names(data$data)
  expected_cols <- c("AllSites", "SiteVisit", "Disturbances", "Observations", "Pests")
  expect_equal(actual_cols, expected_cols)

  # Check that the first object in the data list is a database
  returnType <- data$data[[1]]
  expect_s3_class(returnType, "data.frame")
})
