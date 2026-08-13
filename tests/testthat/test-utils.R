
# MOJN Wrangle function tests ----
# Read in saved MOJN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
mojn_wrangle_test_data <- test_path("mojn_wrangle_test_data.rds")

if (file.exists(mojn_wrangle_test_data)) {
  mojn_wrangled <- readRDS(mojn_wrangle_test_data)
}

# wrangleSites
test_that("wrangleSites works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleSite, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_sites <- wrangleSites(mojn_wrangled)

  # Test if trailing underscores are removed from column names
  expect_false(any(stringr::str_detect(names(mojn_wrangled_sites), "_$")))

  # Test that all shift values are expanded
  expect_true(all(unique(mojn_wrangled_sites$data$AllSites$shift) %in% c("Not Shifted", "North", "South", "East", "West", NA_character_)))

  # Test new geographic columns
  expect_true(all(unique(mojn_wrangled_sites$data$AllSites$verbatimCoordinateSystem) %in% c("UTM", NA_character_)))
  expect_true(all(unique(mojn_wrangled_sites$data$AllSites$verbatimSRS) %in% c("EPSG:4269", NA_character_)))
  expect_true(all(unique(mojn_wrangled_sites$data$AllSites$geodeticDatum) %in% c("EPSG:4326", NA_character_)))

  rows_w_coord <- mojn_wrangled_sites$data$AllSites %>% dplyr::filter(!is.na(verbatimCoordinates))
  expect_true(all(stringr::str_detect(rows_w_coord$verbatimCoordinates[rows_w_coord$Park == "GRBA"], "^11N ")))
  expect_true(all(stringr::str_detect(rows_w_coord$verbatimCoordinates[rows_w_coord$Park == "PARA"], "^12N ")))
  expect_true(all(mojn_wrangled_sites$data$AllSites$verbatimCoordinateSystem == "UTM"))
  expect_true(all(mojn_wrangled_sites$data$AllSites$verbatimSRS == "EPSG:4269"))
  expect_true(all(mojn_wrangled_sites$data$AllSites$geodeticDatum == "EPSG:4326"))

  # Test column additions/renames are present
  expect_true(all(c("shift", "verbatimCoordinates", "verbatimCoordinateSystem", "verbatimSRS", "geodeticDatum", "standHeightInMeters",
                    "decimalLatitude", "decimalLongitude", "elevationInMeters", "slopeInPercent", "aspectInDegrees") %in% names(mojn_wrangled_sites$data$AllSites)))
})

# wrangleSiteVisit
test_that("wrangleSiteVisit works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleSiteVisit, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_sitevisit <- mojn_wrangled %>% wrangleSites() %>% wrangleSiteVisit()

  # Test that park codes and protocol version values are expanded
  expect_true(all(unique(mojn_wrangled_sitevisit$data$SiteVisit$protocolVersion) %in% c("Aspen PIP 1.0", NA_character_))) # only 1 value in lookup tbl
  expect_true(all(unique(mojn_wrangled_sitevisit$data$SiteVisit$unitName) %in% c("Great Basin National Park", "Grand Canyon-Parashant National Monument", NA_character_)))

  # Test column additions/renames are present
  joined_names <- lubridate::setdiff(names(mojn_wrangled_sitevisit$data$AllSites), c("Park", "Site", "ParkName"))
  expect_true(all(c(joined_names, "eventDate", "protocolVersion", "unitCode", "unitName", "siteID", "recordedBy") %in% names(mojn_wrangled_sitevisit$data$SiteVisit)))
  })

# wrangleDisturbance
test_that("wrangleDisturbances works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleDisturbances, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_disturbance <- mojn_wrangled %>% wrangleSites() %>% wrangleSiteVisit() %>% wrangleDisturbances()

  # Test that disturbance codes are expanded
  expect_true(all(unique(mojn_wrangled_disturbance$data$Disturbance$disturbance) %in% c("Antler rubbing", "Fire", "Livestock grazing", "Wildlife grazing", "Arborglyph", NA_character_)))

  # Test column additions/renames are present
  expect_true(all(c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "disturbance") %in% names(mojn_wrangled_disturbance$data$Disturbance)))
  })

# MOJN loadAndWrangle tests ----
# Read in saved load and wrangle output
mojn_loaded_wrangled <- test_path("mojn_loaded_wrangled.rds")

if (file.exists(mojn_loaded_wrangled)) {
  mojn_output <- readRDS(mojn_loaded_wrangled)
}

test_that("loadAndWrangleMOJNAspen() works", {
  skip_if_not(file.exists(mojn_loaded_wrangled), "Skipping tests for loadAndWrangleMOJNAspen, MOJN data for testing not available.")

  # Test that fx only returns data
  expect_equal(names(mojn_output), "data")

  # Test that dataframes have expected names
  expect_equal(names(mojn_output$data), c("SiteVisit", "Disturbances", "Observations", "Pests"))

  # Test that all are dataframes
  lapply(mojn_output$data, expect_s3_class, "data.frame")
})
