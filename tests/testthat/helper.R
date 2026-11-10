# Saved data for load and package tests ----
# Read in saved MOJN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
mojn_wrangle_test_data <- test_path("mojn_wrangle_test_data.rds")

if (file.exists(mojn_wrangle_test_data)) {
  mojn_raw <- readRDS(mojn_wrangle_test_data)

  # Create data for testing each fx
  mojn_wrangled_sites <- wrangle_mojn_sites(mojn_raw)
  mojn_wrangled_sitevisit <- wrangle_mojn_site_visit(mojn_wrangled_sites)
  mojn_wrangled_disturbance <- wrangle_mojn_disturbances(mojn_wrangled_sitevisit)
  mojn_wrangled_observations <- wrangle_mojn_observations(mojn_wrangled_disturbance)
  mojn_wrangled_pests <- wrangle_mojn_pests(mojn_wrangled_observations)
  mojn_packaged <- package_aspen(mojn_wrangled_pests)
}


# Read in saved UCBN data for testing - object has both db and sites tbls and went through fetchagol::cleanData()
ucbn_wrangle_test_data <- test_path("ucbn_wrangle_test_data.rds")

if (file.exists(ucbn_wrangle_test_data)) {
  ucbn_raw <- readRDS(ucbn_wrangle_test_data)

  # Create data for testing each fx
  ucbn_wrangled_sites <- wrangle_ucbn_sites(ucbn_raw)
  ucbn_wrangled_sitevisit <- wrangle_ucbn_site_visit(ucbn_wrangled_sites)
  ucbn_wrangled_disturbance <- wrangle_ucbn_disturbances(ucbn_wrangled_sitevisit)
  ucbn_wrangled_observations <- wrangle_ucbn_observations(ucbn_wrangled_disturbance)
  ucbn_wrangled_pests <- wrangle_ucbn_pests(ucbn_wrangled_observations)
  ucbn_packaged <- package_aspen(ucbn_wrangled_pests)
}

# Wrangled data for other tests ----
# Load once for use in multiple tests
mojn_data <- load_and_package_aspen(network = "MOJN")
ucbn_data <- load_and_package_aspen(network = "UCBN")
