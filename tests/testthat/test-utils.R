# Helper tests
test_that(".get_network works for raw data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping raw data tests for .get_network, MOJN data for testing function not available.")
  # Test raw
  raw_mojn <- .get_network(mojn_raw, raw = TRUE)
  expect_equal(raw_mojn, "MOJN")
  raw_ucbn <- .get_network(ucbn_raw, raw = TRUE)
  expect_equal(raw_ucbn, "UCBN")
})

test_that(".get_network works for wrangled data", {
  # Test packaged
  mojn <- .get_network(mojn_data)
  expect_equal(mojn, "MOJN")
  ucbn <- .get_network(ucbn_data)
  expect_equal(ucbn, "UCBN")
})

# Write tests
test_that("writeAspen creates folder and CSV files", {
  test_aspen1 <- list(
    SiteVisit = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", observer = "KAS"),
    Observations = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", speciesCode = "POTR5" )
  )

  expect_error(writeAspen(test_aspen1),
               regexp = "Data must be in the format returned by `loadAndWrangleAspen()`.",
               fixed = TRUE)

  test_aspen2 <- list(
    data = list(
      SiteVisit = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", observer = "KAS"),
      Observations = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", speciesCode = "POTR5" )
    )
  )

  folder <- here::here("aspen_data_package")

  if (dir.exists(folder)) {
    unlink(folder, recursive = TRUE)
  }

  writeAspen(test_aspen2)

  expect_true(dir.exists(folder))
  expect_true(file.exists(file.path(folder, "MOJN_Aspen_SiteVisit.csv")))
  expect_true(file.exists(file.path(folder, "MOJN_Aspen_Observations.csv")))
  SiteVisit <- read.csv(file.path(folder, "MOJN_Aspen_SiteVisit.csv"))
  expect_equal(nrow(SiteVisit), 1)
  expect_equal(SiteVisit$unitCode, "GRBA")

  unlink(folder, recursive = TRUE)
})
