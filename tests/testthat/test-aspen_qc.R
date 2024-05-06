
# things to test in qc functions

  # check that each data table has the correct number of columns and that the columns have the right names
  # check that the return type from function is correct
  # make fake data and then test number of rows?? - nah probs not


data <- fetchAndWrangleMOJNAspen()

test_that("Test that missingTreeQC() works", {
  # Run function
  returnData <- missingTreeQC(data)

  # Compare expected and actual column names
  actual_cols <- names(returnData)
  expected_cols <- c("Park", "Site", "VisitDate", "SpeciesCode", "Class1", "Class2", "Class3", "Class4", "Class5", "Class6", "totalTreeCount")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  expect_s3_class(returnData, "data.frame")
})

test_that("Test that treeSpeciesQC() works", {
  # Run function
  returnData <- treeSpeciesQC(data)

  # Compare expected and actual column names
  actual_cols <- names(returnData)
  expected_cols <- c("Park", "Site", "VisitDate", "SpeciesCode", "Class1", "Class2", "Class3", "Class4", "Class5", "Class6")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  expect_s3_class(returnData, "data.frame")
})

test_that("Test that unknownSpeciesQC() works", {
  # Run function
  returnData <- unknownSpeciesQC(data)

  # Compare expected and actual column names
  actual_cols <- names(returnData)
  expected_cols <- c("Park", "Site", "VisitDate", "SpeciesCode", "Class1", "Class2", "Class3", "Class4", "Class5", "Class6")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  expect_s3_class(returnData, "data.frame")
})

test_that("Test that checkAspenQC() works", {
  # Run function
  returnData <- checkAspenQC(data)

  # Compare expected and actual column names
  actual_cols <- names(returnData)
  expected_cols <- c("Park", "Site","VisitType", "VisitDate", "SpeciesCode")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  expect_s3_class(returnData, "data.frame")
})
