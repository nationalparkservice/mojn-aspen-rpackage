#' Check that all site visits have at least one observation
#'
#' @param aspen_data List of aspen dataframes in data package format, as returned by loadAndPackageAspen
#'
#' @return Table containing the site visits that do not have at least one observation
checkSiteVisits <- function(aspen_data) {
  visits <- aspen_data$data$SiteVisit %>% dplyr::distinct(siteID, eventDate)
  obs <- aspen_data$data$Observations %>% dplyr::distinct(siteID, eventDate)

  # Identify site visits that are not in the Observations tbl
  visits_without_obs <- dplyr::setdiff(visits, obs)

  select_cols <- c("unitCode", "siteID", "eventDate", "stratum", "zone", "visitType", "visitNotes")

  tbl <- dplyr::semi_join(aspen_data$data$SiteVisit,
                          visits_without_obs,
                          by = c("siteID", "eventDate")) %>%
    dplyr::select(dplyr::any_of(select_cols))

  return(tbl)
}

#' Check for tree records in the Observations table that have a count of zero or a count greater than 250
#'
#' @inheritParams checkSiteVisits
#'
#' @return Table containing tree records that have a count of zero or a count greater than 250
checkTreeCount <- function(aspen_data) {
  # Calculate total count by tree for each site visit
  tree_counts <- aspen_data$data$Observations %>%
    dplyr::group_by(siteID, eventDate, scientificName) %>%
    dplyr::summarise(totalTreeCount = sum(individualCount),
                     .groups = "drop") %>%
    # Filter to zero or >250 counts
    dplyr::filter(totalTreeCount < 1 | totalTreeCount > 250)

  return(tree_counts)
}

#' Check for duplicate tree records in the Observations table
#'
#' @inheritParams checkSiteVisits
#'
#' @return Table containing duplicate tree records in the Observations table
checkDuplicateTrees <- function(aspen_data) {
  dup_trees <- aspen_data$data$Observations %>%
    dplyr::group_by(siteID, eventDate, scientificName) %>%
    dplyr::summarise(rows = n(), .groups = "drop") %>%
    dplyr::filter(rows > 6)

  if (nrow(dup_trees) < 1) {
    tbl <- data.frame()
  } else {
    tbl <- aspen_data$data$Observations %>%
      dplyr::semi_join(dup_trees %>%
                         dplyr::select(-rows),
                       by = c("siteID", "eventDate", "scientificName")) %>%
      dplyr::select(unitCode, siteID, eventDate, speciesCode, scientificName, sizeClass, individualCount)
  }
  return(tbl)
}

#' Check that all tree species have been identified
#'
#' @inheritParams checkSiteVisits
#'
#' @return Table containing rows that are missing either a species code, verbatim identification, or scientific name
checkTreeID <- function(aspen_data) {
  aspen_data$data$Observations %>%
    dplyr::filter(is.na(speciesCode) | is.na(verbatimIdentification) | is.na(scientificName)) %>%
    dplyr::select(unitCode, siteID, eventDate, speciesCode, verbatimIdentification, scientificName)
}
