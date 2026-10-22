# Create test data for checks ----
test_data_fail <- list(
  data = list(
    SiteVisit = dplyr::tribble(
      ~unitCode, ~siteID,      ~eventDate,   ~stratum,     ~zone,                   ~visitType,  ~fieldSeason,  ~observer,  ~visitNotes,
      "GRBA",    "GRBA_A_063", "2021-09-03", "Lexington",  "Lexington Creek",       "Primary",   "GRBA_2021",   "SJU",      "No matching obs",
      "GRBA",    "GRBA_A_063", "2021-09-04", "Lexington",  "Lexington Creek",       "Duplicate", "GRBA_2021",   "SJU",      "Duplicate site visit",
      "GRBA",    "GRBA_A_390", "2022-07-11", "Strawberry", "Strawberry Creek",      "Primary",   "GRBA_2022",   "HTP",       "Missing tree ID with live trees",
      "GRBA",    "GRBA_A_460", "2023-09-12", "West Snake", "Pine and Ridge Creeks", "Primary",   "GRBA_2023",   "KUY",       "Missing tree ID without live trees",
      "GRBA",    "GRBA_A_143", "2023-09-06", "Snake",      "Snake Creek",           "Primary",   "GRBA_2023",   "RW",       "0 count",
      "GRBA",    "GRBA_A_061", "2026-07-11", "Lexington",  "South Fork Big Wash",   "Primary",   "GRBA_2026",   "QQ",       "Duplicate tree records, >250 count"
    ),

    Observations = dplyr::tribble(
      ~unitCode, ~siteID,  ~eventDate,   ~speciesCode, ~verbatimIdentification, ~scientificName,       ~sizeClass,  ~individualCount,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class I",   0,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class II",  0,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class III", 0,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class IV",  0,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class V",   0,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class VI",  0,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class I",   0,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class II",  3,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class III", 0,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class IV",  0,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class V",   0,
      "GRBA",  "GRBA_A_390", "2022-07-11", NA,           NA,                      NA,                    "Class VI",  0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class I",   0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class II",  0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class III", 0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class IV",  0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class V",   0,
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",        "Unknown",               NA,                    "Class VI",  1,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class I",   50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class II",  50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class III", 50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class IV",  50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class V",   50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class VI",  50,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class I",   4,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class II",  0,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class III", 7,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class IV",  22,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class V",   6,
      "GRBA",  "GRBA_A_061", "2026-07-11", "POTR5",      "Populus tremuloides",   "Populus tremuloides", "Class VI",  3
    ),

    Pests = dplyr::tribble(
      ~unitCode, ~siteID,  ~eventDate,   ~speciesCode, ~scientificName, ~pest,
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5", "Populus tremuloides", "Canker",
      "GRBA",  "GRBA_A_143", "2023-09-06", "POTR5", "Populus tremuloides", "Canker",
      "GRBA",  "GRBA_A_460", "2023-09-12", "UNK",   "Unknown", "Mountain Pine Beetle"
    )
  )
)

test_data_pass <- list(data = lapply(ucbn_data$data,
                                     dplyr::filter, siteID %in% c("CIRO_11_2_1", "CIRO_24_1_1", "CIRO_54_1_2")))

# Test QC functions ----
test_that("checkSiteVisits works for pass and fail", {
  # test fail behavior (tbl output)
  fail <- checkSiteVisits(test_data_fail)

  expect_equal(nrow(fail), 2)
  expect_equal(ncol(fail), 7) # MOJN col names
  expect_setequal(names(fail), c("unitCode", "siteID", "eventDate", "stratum", "zone", "visitType", "visitNotes"))
  expect_equal(unique(fail$siteID), "GRBA_A_063")

  # test pass behavior
  pass <- checkSiteVisits(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(pass), 5) # UCBN col names
  expect_setequal(names(pass), c("unitCode", "siteID", "eventDate", "visitType", "visitNotes"))
})

test_that("checkDuplicateSiteVisits works for pass and fail", {
  # test fail behavior (tbl output)
  fail <- checkDuplicateSiteVisits(test_data_fail)

  expect_equal(nrow(fail), 2)
  expect_equal(unique(fail$siteID), "GRBA_A_063")
  expect_equal(ncol(fail), 7) # MOJN col names
  expect_equal(names(fail), c("unitCode", "siteID", "year", "eventDate", "visitType",
                              "fieldSeason", "visitNotes"))

  # test pass behavior
  pass <- checkDuplicateSiteVisits(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(pass), 9) # UCBN col names
  expect_equal(names(pass), c("unitCode", "siteID", "year", "eventDate", "visitType",
                              "stand", "transect", "plotNumber", "visitNotes"))
})

test_that("checkSiteAspen works for pass and fail", {
  # test fail behavior
  fail <- checkSiteAspen(test_data_fail)

  expect_equal(nrow(fail), 5)
  expect_equal(unique(fail$siteID), c("GRBA_A_063", "GRBA_A_390", "GRBA_A_460", "GRBA_A_143"))
  expect_equal(ncol(fail), 7) # MOJN col names
  expect_equal(names(fail), c("unitCode", "siteID", "eventDate", "stratum", "zone", "visitType", "visitNotes"))

  # test pass behavior
  pass <- checkSiteAspen(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(pass), 5) # UCBN col names
  expect_setequal(names(pass), c("unitCode", "siteID", "eventDate", "visitType", "visitNotes"))
})

test_that("checkTreeCount works for pass and fail", {
  # test fail behavior
  fail <- checkTreeCount(test_data_fail)

  expect_equal(nrow(fail), 2)
  expect_equal(ncol(fail), 4)
  expect_setequal(c("GRBA_A_061", "GRBA_A_143"), unique(fail$siteID))
  expect_all_true(fail$totalTreeCount < 1 | fail$totalTreeCount > 250)

  # test pass behavior
  pass <- checkTreeCount(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(fail), ncol(pass))
  expect_equal(names(fail), names(pass))
})

test_that("checkDuplicateTrees works for pass and fail", {
  # test fail behavior
  fail <- checkDuplicateTrees(test_data_fail)

  expect_equal(nrow(fail), 12)
  expect_all_true(fail$siteID == "GRBA_A_061")
  expect_all_true(fail$scientificName == "Populus tremuloides")
  expect_setequal(names(fail), c("unitCode", "siteID", "eventDate", "speciesCode", "scientificName", "sizeClass", "individualCount"))

  # test pass behavior
  pass <- checkDuplicateTrees(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(fail), ncol(pass))
  expect_equal(names(fail), names(pass))
})

test_that("checkTreeID works for pass and fail", {
  # test fail behavior
  fail <- checkTreeID(test_data_fail)

  expect_equal(nrow(fail), 2)
  expect_true(fail$hasLiveTree[fail$siteID == "GRBA_A_390"])
  expect_false(fail$hasLiveTree[fail$siteID == "GRBA_A_460"])
  expect_setequal(names(fail), c("unitCode", "siteID", "eventDate", "speciesCode", "verbatimIdentification", "scientificName", "hasLiveTree"))

  # test pass behavior
  pass <- checkTreeID(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(fail), ncol(pass))
  expect_equal(names(fail), names(pass))
})

test_that("checkDuplicatePests works for pass and fail", {
  # test fail behavior
  fail <- checkDuplicatePests(test_data_fail)

  expect_equal(nrow(fail), 2)
  expect_equal(ncol(fail), 6) # MOJN col names
  expect_setequal(names(fail), c("unitCode", "siteID", "eventDate", "speciesCode", "scientificName", "pest"))

  # test pass behavior
  pass <- checkDuplicatePests(test_data_pass)

  expect_equal(nrow(pass), 0)
  expect_equal(ncol(pass), 9) # UCBN col names
  expect_equal(names(pass), c("unitCode", "siteID", "eventDate", "stand", "transect",
                              "plotNumber", "speciesCode", "scientificName", "pest"))
})
