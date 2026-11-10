# Saved data for load and package tests ----
# Read in saved MOJN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
mojn_wrangle_test_data <- test_path("mojn_wrangle_test_data.rds")

if (file.exists(mojn_wrangle_test_data)) {
  mojn_raw <- readRDS(mojn_wrangle_test_data)

  # Create data for testing each fx
  mojn_wrangled_sites <- wrangleMOJNSites(mojn_raw)
  mojn_wrangled_sitevisit <- wrangleMOJNSiteVisit(mojn_wrangled_sites)
  mojn_wrangled_disturbance <- wrangleMOJNDisturbances(mojn_wrangled_sitevisit)
  mojn_wrangled_observations <- wrangleMOJNObservations(mojn_wrangled_disturbance)
  mojn_wrangled_pests <- wrangleMOJNPests(mojn_wrangled_observations)
  mojn_packaged <- packageAspen(mojn_wrangled_pests)
}


# Read in saved UCBN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
ucbn_wrangle_test_data <- test_path("ucbn_wrangle_test_data.rds")

if (file.exists(ucbn_wrangle_test_data)) {
  ucbn_raw <- readRDS(ucbn_wrangle_test_data)

  # Create data for testing each fx
  ucbn_wrangled_sites <- wrangleUCBNSites(ucbn_raw)
  ucbn_wrangled_sitevisit <- wrangleUCBNSiteVisit(ucbn_wrangled_sites)
  ucbn_wrangled_disturbance <- wrangleUCBNDisturbances(ucbn_wrangled_sitevisit)
  ucbn_wrangled_observations <- wrangleUCBNObservations(ucbn_wrangled_disturbance)
  ucbn_wrangled_pests <- wrangleUCBNPests(ucbn_wrangled_observations)
  ucbn_packaged <- packageAspen(ucbn_wrangled_pests)
}

# Wrangled data for other tests ----
# Load once for use in multiple tests
mojn_data <- loadAndPackageAspen(network = "MOJN")
ucbn_data <- loadAndPackageAspen(network = "UCBN")
