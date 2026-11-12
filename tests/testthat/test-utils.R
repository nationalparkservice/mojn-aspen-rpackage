# Helper tests
testthat::test_that(".get_network works for raw data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping raw data tests for .get_network, MOJN data for testing function not available.")
  # Test raw
  raw_mojn <- .get_network(mojn_raw, raw = TRUE)
  expect_equal(raw_mojn, "MOJN")
  raw_ucbn <- .get_network(ucbn_raw, raw = TRUE)
  expect_equal(raw_ucbn, "UCBN")
})

testthat::test_that(".get_network works for wrangled data", {
  # Test packaged
  mojn <- .get_network(mojn_data)
  expect_equal(mojn, "MOJN")
  ucbn <- .get_network(ucbn_data)
  expect_equal(ucbn, "UCBN")
})

testthat::test_that("fetch_ucbn_eas works for saved data", {
  # Create temp folder
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  cache_file <- file.path(tools::R_user_dir("aspen", "data"), "ucbn_elevation_aspect_slope.rds")
  dir.create(dirname(cache_file), recursive = TRUE)

  # Saved data
  saveRDS(tibble::tibble(decimalLatitude = 42.07, decimalLongitude = -113.71,
                         Elevation = 1000, Slope = 10, Aspect = 180),
          cache_file)

  # Test that uses saved values for same coordinates
  df <- tibble::tibble(decimalLatitude = 42.07, decimalLongitude = -113.71, latlong_wkid = "4326")
  res <- fetch_ucbn_eas(df, agol_username = "test")

  expect_equal(res$Elevation, 1000)
  expect_equal(res$Slope, 10)
  expect_equal(res$Aspect, 180)
})

# Write tests
testthat::test_that("write_aspen creates folder and CSV files", {
  test_aspen1 <- list(
    SiteVisit = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", observer = "KAS"),
    Observations = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", speciesCode = "POTR5" )
  )

  expect_error(write_aspen(test_aspen1),
               regexp = "Data must be in the format returned by `load_and_package_aspen()`.",
               fixed = TRUE)

  test_aspen2 <- list(
    data = list(
      SiteVisit = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", observer = "KAS"),
      Observations = data.frame(unitCode = "GRBA", eventDate = "2021-09-15", speciesCode = "POTR5" )
    )
  )

  folder <- here::here("aspen_data")

  if (dir.exists(folder)) {
    unlink(folder, recursive = TRUE)
  }

  write_aspen(test_aspen2)

  expect_true(dir.exists(folder))
  expect_true(file.exists(file.path(folder, "MOJN_Aspen_SiteVisit.csv")))
  expect_true(file.exists(file.path(folder, "MOJN_Aspen_Observations.csv")))
  SiteVisit <- read.csv(file.path(folder, "MOJN_Aspen_SiteVisit.csv"))
  expect_equal(nrow(SiteVisit), 1)
  expect_equal(SiteVisit$unitCode, "GRBA")

  unlink(folder, recursive = TRUE)
})
