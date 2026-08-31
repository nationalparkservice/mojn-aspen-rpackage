#' Check that all site visits have at least one observation
#'
#' @param aspen_data Nested list of aspen dataframes in data package format, as returned by loadAndPackageAspen
#'
#' @return Table containing site visits that do not have at least one observation
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

#' Check for tree records in the Observations table that have a total count of zero or a total count greater than 250
#'
#' @inheritParams checkSiteVisits
#'
#' @return Table containing tree records that have a total count of zero or a total count greater than 250
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
    dplyr::filter(rows > 6) # six size classes = one tree record, multiples of 6 = duplicate

  tbl <- aspen_data$data$Observations %>%
    dplyr::semi_join(dup_trees %>%
                       dplyr::select(-rows),
                     by = c("siteID", "eventDate", "scientificName")) %>%
    dplyr::select(unitCode, siteID, eventDate, speciesCode, scientificName, sizeClass, individualCount)

  return(tbl)
}

#' Check for trees have not been identified
#'
#' The column hasLiveTree in the results table is TRUE when an unidentified tree
#' has live trees recorded and is FALSE otherwise. When a dead tree is encountered
#' in the field, it may not be possible to identify it, but all live trees should
#' be identified.
#'
#' @inheritParams checkSiteVisits
#'
#' @return Table containing rows that are missing tree identification and an indication of whether the record has live trees
checkTreeID <- function(aspen_data) {
  tbl <- aspen_data$data$Observations %>%
    dplyr::filter(is.na(speciesCode) | is.na(verbatimIdentification) | is.na(scientificName)) %>%
    dplyr::select(unitCode, siteID, eventDate, speciesCode, verbatimIdentification, scientificName, sizeClass, individualCount)  %>%
    dplyr::group_by(unitCode, siteID, eventDate, speciesCode, verbatimIdentification, scientificName) %>%
    dplyr::summarise(hasLiveTree = any(sizeClass %in% c("Class I", "Class II", "Class III", "Class IV", "Class V") & individualCount > 0),
                     .groups = "drop")

  return(tbl)
}
