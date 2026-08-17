
# MOJN Wrangle and Package function tests ----
# Read in saved MOJN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
mojn_wrangle_test_data <- test_path("mojn_wrangle_test_data.rds")

if (file.exists(mojn_wrangle_test_data)) {
  mojn_data <- readRDS(mojn_wrangle_test_data)
}

# wrangleSites
test_that("wrangleSites works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleSite, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_sites <- wrangleSites(mojn_data)

  # Test if trailing underscores are removed from column names
  expect_false(any(stringr::str_detect(names(mojn_wrangled_sites), "_$")))

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

# wrangleSiteVisit
test_that("wrangleSiteVisit works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleSiteVisit, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_sitevisit <- mojn_data %>% wrangleSites() %>% wrangleSiteVisit()

  # Test that park codes and protocol version values are expanded
  expect_all_true(unique(mojn_wrangled_sitevisit$data$SiteVisit$protocolVersion) %in% c("Aspen PIP 1.0", NA_character_)) # only 1 value in lookup tbl
  expect_all_true(unique(mojn_wrangled_sitevisit$data$SiteVisit$unitName) %in% c("Great Basin National Park", "Grand Canyon-Parashant National Monument", NA_character_))

  # Test column additions/renames are present
  joined_names <- lubridate::setdiff(names(mojn_wrangled_sitevisit$data$AllSites), c("Park", "Site", "ParkName"))
  expect_contains(names(mojn_wrangled_sitevisit$data$SiteVisit), c(joined_names, "eventDate", "protocolVersion", "unitCode", "unitName", "siteID", "recordedBy"))
  })

# wrangleDisturbance
test_that("wrangleDisturbances works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleDisturbances, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_disturbance <- mojn_data %>% wrangleSites() %>% wrangleSiteVisit() %>% wrangleDisturbances()

  # Test that disturbance codes are expanded
  expect_all_true(unique(mojn_wrangled_disturbance$data$Disturbance$disturbance) %in% c("Antler rubbing", "Fire", "Livestock grazing", "Wildlife grazing", "Arborglyph", NA_character_))

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_disturbance$data$Disturbance), c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "disturbance"))
  })


# wrangleObservations
test_that("wrangleObservations works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wrangleObservations, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_observations <- mojn_data %>% wrangleSites() %>% wrangleSiteVisit() %>% wrangleDisturbances() %>% wrangleObservations()

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

# wranglePests
test_that("wranglePests works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for wranglePests, MOJN data for testing wrangle functions not available.")
  mojn_wrangled_pests <- mojn_data %>% wrangleSites() %>% wrangleSiteVisit() %>% wrangleDisturbances() %>% wrangleObservations() %>% wranglePests()

  # Test column additions/renames are present
  expect_contains(names(mojn_wrangled_pests$data$Pests), c("unitCode", "unitName", "siteID", "eventDate", "VisitType", "Community", "SpeciesCode", "verbatimIdentification", "pest"))

  # Test that col globalid is removed from observations tbl
  expect_false("globalid" %in% names(mojn_wrangled_pests$data$Observations))
})

# packageMOJNAspen
test_that("packageMOJNAspen works", {
  skip_if_not(file.exists(mojn_wrangle_test_data), "Skipping tests for packageMOJNAspen, MOJN data for testing wrangle and package functions not available.")
  mojn_packaged <- mojn_data %>% wrangleSites() %>% wrangleSiteVisit() %>% wrangleDisturbances() %>% wrangleObservations() %>% wranglePests() %>% packageMOJNAspen()

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
  expect_named(mojn_output$data, c("SiteVisit", "Disturbances", "Observations", "Pests"))

  # Test that all are dataframes
  lapply(mojn_output$data, expect_s3_class, "data.frame")
})

# UCBN Wrangle and Package function tests ----
# Read in saved MOJN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
ucbn_wrangle_test_data <- test_path("ucbn_wrangle_test_data.rds")

if (file.exists(ucbn_wrangle_test_data)) {
  ucbn_data <- readRDS(ucbn_wrangle_test_data)
}

# wrangleUCBNSites
test_that("wrangleUCBNSites works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangleUCBNSites, UCBN data for testing wrangle functions not available.")
  ucbn_wrangled_sites <- wrangleUCBNSites(ucbn_data)

  # Test column additions/renames are present
  expect_equal(names(ucbn_wrangled_sites$data$Locations), c("Loc_Name", "verbatimCoordinateSystem", "verbatimSRS", "verbatimCoordinates", "geodeticDatum", "decimalLatitude", "decimalLongitude", "elevationInMeters", "slopeInPercent", "aspectInDegrees"))
})

# wrangleUCBNSiteVisist
test_that("wrangleUCBNSiteVisit works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangleUCBNSiteVisit, UCBN data for testing wrangle functions not available.")
  ucbn_wrangled_sitevisit <- ucbn_data %>% wrangleUCBNSites() %>% wrangleUCBNSiteVisit()

  # Test codes expanded
  expect_true(unique(ucbn_wrangled_sitevisit$data$SiteVisit$unitName) %in% c("City of Rocks National Reserve", "Craters of the Moon National Monument and Preserve"))
  expect_true(unique(ucbn_wrangled_sitevisit$data$SiteVisit$protocolVersion) %in% "ASPN_1_0")

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_sitevisit$data$SiteVisit), c("globalid", "unitCode", "unitName", "Stand", "Transect", "plotNumber", "plotName", "eventDate", "standHeightInMeters", "verbatimCoordinateSystem", "aspectInDegrees", "protocolVersion", "VisitNotes"))
})

# wrangleUCBNDisturbances
test_that("wrangleUCBNDisturbances works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangleUCBNDisturbances, UCBN data for testing wrangle functions not available.")
  ucbn_wrangled_disturbance <- ucbn_data %>% wrangleUCBNSites() %>% wrangleUCBNSiteVisit() %>% wrangleUCBNDisturbances()

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_disturbance$data$Disturbance), c("unitCode", "unitName", "Stand", "Transect", "plotNumber", "plotName", "eventDate", "Disturbance"))
})

# wrangleUCBNObservations
test_that("wrangleUCBNObservations works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangleUCBNObservations, UCBN data for testing wrangle functions not available.")
  ucbn_wrangled_observations <- ucbn_data %>% wrangleUCBNSites() %>% wrangleUCBNSiteVisit() %>% wrangleUCBNDisturbances() %>% wrangleUCBNObservations()

  # Test expanding size classes and sci names
  expect_setequal(unique(ucbn_wrangled_observations$data$Observations$sizeClass), c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI"))
  expect_false(any(is.na(ucbn_wrangled_observations$data$Observations$sizeClass)))
  expect_false(any(is.na(ucbn_wrangled_observations$data$Observations$sizeClassDescription)))
  expect_false(any(stringr::str_detect(unique(ucbn_wrangled_observations$data$Observations$verbatimIdentification), "\\(|\\)"), na.rm = TRUE))

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_observations$data$Observations), c("unitCode", "unitName", "Stand", "Transect", "plotNumber", "plotName", "eventDate", "verbatimIdentification", "sizeClass", "individualCount", "sizeClassDescription"))

  # Test that col globalid is removed from SiteVisits tbl
  expect_false("globalid" %in% names(ucbn_wrangled_observations$data$SiteVisit))
})

# wrangleUCBNPests
test_that("wrangleUCBNObservations works", {
  skip_if_not(file.exists(ucbn_wrangle_test_data), "Skipping tests for wrangleUCBNObservations, UCBN data for testing wrangle functions not available.")
  ucbn_wrangled_pests <- ucbn_data %>% wrangleUCBNSites() %>% wrangleUCBNSiteVisit() %>% wrangleUCBNDisturbances() %>% wrangleUCBNObservations() %>% wrangleUCBNPests()

  # Test pest code expansions
  expect_all_true(unique(ucbn_wrangled_pests$data$Pests$pest) %in% c("Cankers", "Bark/Wood Boring Insect", "Defoliating Insect", "Foliage Disease", "Dwarf Mistletoe", "Mountain Pine Beetle", "Stem Decay", "White Pine Blister Rust"))

  # Test column additions/renames are present
  expect_contains(names(ucbn_wrangled_pests$data$Pests), c("unitCode", "unitName", "Stand", "Transect", "plotNumber", "plotName", "eventDate", "verbatimIdentification", "pest", "pestDescription"))

  # Test that col globalid is removed from observations tbl
  expect_false("globalid" %in% names(ucbn_wrangled_pests$data$Observations))
})
