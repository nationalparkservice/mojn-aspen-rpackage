# MOJN tests ----

# wrangle_mojn_sites
testthat::test_that("wrangle_mojn_sites works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_mojn_sites, MOJN data for testing wrangle functions not available.")

  # Test if trailing underscores are removed from column names
  expect_false(any(stringr::str_detect(names(mojn_wrangled_sites$data$AllSites), "_$")))

  # Test that all shift values are expanded
  expect_all_true(unique(mojn_wrangled_sites$data$AllSites$shift) %in% c("Not Shifted", "North", "South", "East", "West", NA_character_))

  # Test new geographic columns
  expect_all_true(unique(mojn_wrangled_sites$data$AllSites$verbatimCoordinateSystem) %in% c("UTM", NA_character_))
  expect_all_true(unique(mojn_wrangled_sites$data$AllSites$verbatimSRS) %in% c("EPSG:4269", NA_character_))
  expect_all_true(unique(mojn_wrangled_sites$data$AllSites$geodeticDatum) %in% c("EPSG:4326", NA_character_))

  rows_w_coord <- mojn_wrangled_sites$data$AllSites %>% dplyr::filter(!is.na(verbatimCoordinates))
  expect_all_true(stringr::str_detect(rows_w_coord$verbatimCoordinates[rows_w_coord$Park == "GRBA"], "^11N "))
  expect_all_true(stringr::str_detect(rows_w_coord$verbatimCoordinates[rows_w_coord$Park == "PARA"], "^12N "))
  expect_all_true(mojn_wrangled_sites$data$AllSites$verbatimCoordinateSystem == "UTM")
  expect_all_true(mojn_wrangled_sites$data$AllSites$verbatimSRS == "EPSG:4269")
  expect_all_true(mojn_wrangled_sites$data$AllSites$geodeticDatum == "EPSG:4326")

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_sites$data$AllSites), c("shift", "verbatimCoordinates", "verbatimCoordinateSystem", "verbatimSRS", "geodeticDatum", "standHeightInMeters", "decimalLatitude", "decimalLongitude", "elevationInMeters", "slopeInPercent", "aspectInDegrees"))
})

# wrangle_mojn_site_visit
testthat::test_that("wrangle_mojn_site_visit works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_mojn_site_visit, MOJN data for testing wrangle functions not available.")

  # Test that park codes and protocol version values are expanded
  expect_all_true(unique(mojn_wrangled_sitevisit$data$SiteVisit$protocolVersion) %in% c("Aspen PIP 1.0", NA_character_)) # only 1 value in lookup tbl
  expect_all_true(unique(mojn_wrangled_sitevisit$data$SiteVisit$unitName) %in% c("Great Basin National Park", "Grand Canyon-Parashant National Monument", NA_character_))

  # Test column additions/renames are present
  joined_names <- lubridate::setdiff(names(mojn_wrangled_sitevisit$data$AllSites), c("Park", "Site", "ParkName"))
  expect_contains(names(mojn_wrangled_sitevisit$data$SiteVisit), c(joined_names, "eventDate", "protocolVersion", "unitCode", "unitName", "siteID", "recordedBy"))
})

# wrangleMOJNDisturbance
testthat::test_that("wrangle_mojn_disturbances works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_mojn_disturbances, MOJN data for testing wrangle functions not available.")

  # Test that disturbance codes are expanded
  expect_all_true(unique(mojn_wrangled_disturbance$data$Disturbance$disturbance) %in% c("Antler rubbing", "Fire", "Livestock grazing", "Wildlife grazing", "Arborglyph", NA_character_))

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_disturbance$data$Disturbance), c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "disturbance"))
})

# wrangle_mojn_observations
testthat::test_that("wrangle_mojn_observations works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_mojn_observations, MOJN data for testing wrangle functions not available.")

  # Test expanding size classes and sci names
  expect_setequal(unique(mojn_wrangled_observations$data$Observations$sizeClass), c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI"))
  expect_false(any(is.na(mojn_wrangled_observations$data$Observations$sizeClass)))
  expect_false(any(is.na(mojn_wrangled_observations$data$Observations$sizeClassDescription)))
  expect_false(any(stringr::str_detect(unique(mojn_wrangled_observations$data$Observations$verbatimIdentification), "\\(|\\)"), na.rm = TRUE))

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_observations$data$Observations), c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "verbatimIdentification", "sizeClass", "individualCount", "sizeClassDescription"))

  # Test that col globalid is removed from SiteVisits tbl
  expect_false("globalid" %in% names(mojn_wrangled_observations$data$SiteVisit))
})

# wrangle_mojn_pests
testthat::test_that("wrangle_mojn_pests works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_mojn_pests, MOJN data for testing wrangle functions not available.")

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_pests$data$Pests), c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "SpeciesCode", "verbatimIdentification", "pest"))

  # Test that col globalid is removed from observations tbl
  expect_false("globalid" %in% names(mojn_wrangled_pests$data$Observations))
})

# UCBN tests ----

# wrangle_ucbn_sites
testthat::test_that("wrangle_ucbn_sites works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_ucbn_sites, UCBN data for testing wrangle functions not available.")

  # Test column additions/renames are present
  expect_equal(names(ucbn_wrangled_sites$data$Locations), c("Loc_Name", "visitType", "verbatimCoordinateSystem", "verbatimSRS", "verbatimCoordinates", "geodeticDatum", "decimalLatitude", "decimalLongitude", "elevationInMeters", "slopeInPercent", "aspectInDegrees"))
})

# wrangleUCBNSiteVisist
testthat::test_that("wrangle_ucbn_site_visit works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_ucbn_site_visit, UCBN data for testing wrangle functions not available.")

  # Test codes expanded
  expect_true(unique(ucbn_wrangled_sitevisit$data$SiteVisit$unitName) %in% c("City of Rocks National Reserve", "Craters of the Moon National Monument and Preserve"))
  expect_true(unique(ucbn_wrangled_sitevisit$data$SiteVisit$protocolVersion) %in% "ASPN_1_0")

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_sitevisit$data$SiteVisit), c("globalid", "unitCode", "unitName", "siteID", "Stand", "Transect", "plotNumber", "eventDate", "standHeightInMeters", "verbatimCoordinateSystem", "aspectInDegrees", "protocolVersion", "VisitNotes"))
})

# wrangle_ucbn_disturbances
testthat::test_that("wrangle_ucbn_disturbances works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_ucbn_disturbances, UCBN data for testing wrangle functions not available.")

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_disturbance$data$Disturbance), c("unitCode", "unitName", "siteID", "Stand", "Transect", "plotNumber", "eventDate", "Disturbance"))
})

# wrangle_ucbn_observations
testthat::test_that("wrangle_ucbn_observations works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_ucbn_observations, UCBN data for testing wrangle functions not available.")

  # Test expanding size classes and sci names
  expect_setequal(unique(ucbn_wrangled_observations$data$Observations$sizeClass), c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI"))
  expect_false(any(is.na(ucbn_wrangled_observations$data$Observations$sizeClass)))
  expect_false(any(is.na(ucbn_wrangled_observations$data$Observations$sizeClassDescription)))
  expect_false(any(stringr::str_detect(unique(ucbn_wrangled_observations$data$Observations$verbatimIdentification), "\\(|\\)"), na.rm = TRUE))

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_observations$data$Observations), c("unitCode", "unitName", "siteID", "Stand", "Transect", "plotNumber", "eventDate", "verbatimIdentification", "sizeClass", "individualCount", "sizeClassDescription"))

  # Test that col globalid is removed from SiteVisits tbl
  expect_false("globalid" %in% names(ucbn_wrangled_observations$data$SiteVisit))
})

# wrangle_ucbn_pests
testthat::test_that("wrangle_ucbn_pests works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_ucbn_observations, UCBN data for testing wrangle functions not available.")

  # Test pest code expansions
  expect_all_true(unique(ucbn_wrangled_pests$data$Pests$pest) %in% c("Cankers", "Bark/Wood Boring Insect", "Defoliating Insect", "Foliage Disease", "Dwarf Mistletoe", "Mountain Pine Beetle", "Stem Decay", "White Pine Blister Rust"))

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_pests$data$Pests), c("unitCode", "unitName", "siteID", "Stand", "Transect", "plotNumber", "eventDate", "verbatimIdentification", "pest", "pestDescription"))

  # Test that col globalid is removed from observations tbl
  expect_false("globalid" %in% names(ucbn_wrangled_pests$data$Observations))
})

# Combined function tests ----
# wrangle_sites
testthat::test_that("wrangle_sites works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_sites, MOJN data for testing wrangle functions not available.")
  expect_equal(wrangle_sites(mojn_raw), mojn_wrangled_sites)
})

testthat::test_that("wrangle_sites works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_sites, UCBN data for testing wrangle functions not available.")
  expect_equal(wrangle_sites(ucbn_raw), ucbn_wrangled_sites)
})

# wrangle_site_visit
testthat::test_that("wrangle_site_visit works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_site_visit, MOJN data for testing wrangle functions not available.")
  expect_equal(wrangle_site_visit(mojn_wrangled_sites), mojn_wrangled_sitevisit)
})

testthat::test_that("wrangle_site_visit works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_site_visit, UCBN data for testing wrangle functions not available.")
  expect_equal(wrangle_site_visit(ucbn_wrangled_sites), ucbn_wrangled_sitevisit)
})

# wrangle_disturbances
testthat::test_that("wrangle_disturbances works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_disturbances, MOJN data for testing wrangle functions not available.")
  expect_equal(wrangle_disturbances(mojn_wrangled_sitevisit), mojn_wrangled_disturbance)
})

testthat::test_that("wrangle_disturbances works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_disturbances, UCBN data for testing wrangle functions not available.")
  expect_equal(wrangle_disturbances(ucbn_wrangled_sitevisit), ucbn_wrangled_disturbance)
})

# wrangle_observations
testthat::test_that("wrangle_observations works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_observations, MOJN data for testing wrangle functions not available.")
  expect_equal(wrangle_observations(mojn_wrangled_disturbance), mojn_wrangled_observations)
})

testthat::test_that("wrangle_observations works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_observations, UCBN data for testing wrangle functions not available.")
  expect_equal(wrangle_observations(ucbn_wrangled_disturbance), ucbn_wrangled_observations)
})

# wrangle_pests
testthat::test_that("wrangle_pests works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangle_pests, MOJN data for testing wrangle functions not available.")
  expect_equal(wrangle_pests(mojn_wrangled_observations), mojn_wrangled_pests)
})

testthat::test_that("wrangle_pests works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangle_pests, UCBN data for testing wrangle functions not available.")
  expect_equal(wrangle_pests(ucbn_wrangled_observations), ucbn_wrangled_pests)
})

# package_aspen
testthat::test_that("package_aspen works for MOJN data", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for package_aspen, MOJN data for testing wrangle functions not available.")

  # Test removal of metadata and sites tbl
  expect_false("metadata" %in% names(mojn_packaged))
  expect_false("AllSites" %in% names(mojn_packaged$data))

  # Test column additions/renames
  for (tbl_name in names(mojn_packaged$data)) {
    tbl <- mojn_packaged$data[[tbl_name]]

    expect_contains(names(tbl), c("type", "basisOfRecord", "siteID"))
    basisOfRecord_value <- ifelse(tbl_name == "SiteVisit", "Event", "HumanObservation")
    expect_all_true(tbl$basisOfRecord == basisOfRecord_value)
  }

  # Test taxonomy changes
  for (tbl in c("Observations", "Pests")) {
    expect_all_true(c("verbatimIdentification", "scientificName", "taxonRank") %in% names(mojn_packaged$data[[tbl]]))
    expect_false("Pinus longeava" %in% unique(mojn_packaged$data[[tbl]]$scientificName))
  }
})

testthat::test_that("package_aspen works for UCBN data", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for package_aspen, UCBN data for testing wrangle functions not available.")

  # Test removal of metadata and tbls
  expect_equal(names(ucbn_packaged), "data")
  expect_all_false(c("begin_image_repeat", "Locations") %in% names(ucbn_packaged$data))

  # Test column additions/renames
  for (tbl_name in names(ucbn_packaged$data)) {
    tbl <- ucbn_packaged$data[[tbl_name]]

    expect_contains(names(tbl), c("type", "basisOfRecord"))
    basisOfRecord_value <- ifelse(tbl_name == "SiteVisit", "Event", "HumanObservation")
    expect_all_true(tbl$basisOfRecord == basisOfRecord_value)
  }

  # Test taxonomy changes
  for (tbl in c("Observations", "Pests")) {
    expect_all_true(c("verbatimIdentification", "scientificName", "taxonRank") %in% names(ucbn_packaged$data[[tbl]]))
  }
})

# load_and_package_aspen
testthat::test_that("load_and_package_aspen() works", {
  expect_error(load_and_package_aspen(network = "mojave desert network", agol_username = "placeholder", agol_password = "placeholder"))

  # Test that fx only returns data
  expect_equal(names(mojn_data), "data")

  # Test that dataframes have expected names
  expect_named(mojn_data$data, c("SiteVisit", "Disturbances", "Observations", "Pests"))

  # Test that all are dataframes
  lapply(mojn_data$data, expect_s3_class, "data.frame")

  # Test that fx only returns data
  expect_equal(names(ucbn_data), "data")

  # Test that dataframes have expected names
  expect_named(ucbn_data$data, c("SiteVisit", "Disturbances", "Observations", "Pests"))

  # Test that all are dataframes
  lapply(ucbn_data$data, expect_s3_class, "data.frame")
})
