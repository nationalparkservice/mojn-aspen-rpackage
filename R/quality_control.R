# Assign global variables to avoid the "no visible binding for global variable 'x'" error in build checks
globalVariables(c(
  "rows", "speciesCode", "sizeClass", "individualCount", "totalTreeCount", "Status",
  "year", "disturbance", "pest", "live"
))

#' Check site visit table for duplicate visits or visits without observations
#'
#' @description
#' `check_site_visits()` performs three tests on the site visit table. When \code{test
#' = "observation"} it returns site visit records without any records in the
#' observations table. When \code{test = "duplicate"} it returns visit records for
#' sites that were visited more than once in a calendar year. When \code{test = "aspen"}
#' it returns site visit records that do not have any live aspen recorded.
#'
#' @param aspen_data Nested list of aspen dataframes in data package format, as returned by `load_and_package_aspen()`
#' @param test Character string representing what test to perform. Must be "observation", "duplicate" or "aspen"
#' @param format_table Logical. TRUE applies reactable table formatting, use for Rmd. Use FALSE for writing to CSV.
#'
#' @returns Table containing site visit records that met the criteria for the specified test
#' @export
check_site_visits <- function(aspen_data, test, format_table = TRUE) {
  select_cols <- c("unitCode", "siteID", "eventDate", "stratum", "zone", "visitType", "visitNotes")
  shade_by <- NULL

  if (test == "observation") {
    # Identify site visits that are not in the Observations tbl
    tbl <- dplyr::anti_join(aspen_data$data$SiteVisit,
                            aspen_data$data$Observations,
                            by = c("siteID", "eventDate", "visitType")) %>%
      dplyr::select(dplyr::any_of(select_cols))

  } else if (test == "duplicate") {
    # Identify sites that were visited more than once in a year
    select_cols <- c("unitCode", "siteID", "year", "eventDate", "visitType",
                     "fieldSeason", "visitNotes")

    tbl <- aspen_data$data$SiteVisit %>%
      dplyr::mutate(year = lubridate::year(eventDate)) %>%
      dplyr::add_count(siteID, year) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(dplyr::any_of(select_cols)) %>%
      dplyr::arrange(siteID, year)

    shade_by <- c("unitCode", "siteID", "year")

  } else if (test == "aspen") {
    # Identify visits where no live aspen were recorded
    live_aspen <- aspen_data$data$Observations %>%
      dplyr::filter(scientificName == "Populus tremuloides" &
                      sizeClass %in% c("Class I", "Class II", "Class III", "Class IV", "Class V") &
                      individualCount > 0) %>%
      dplyr::group_by(siteID, eventDate, visitType) %>%
      dplyr::distinct(siteID, eventDate)

    tbl <- aspen_data$data$SiteVisit %>%
      dplyr::anti_join(live_aspen,
                       by = c("siteID", "eventDate")) %>%
      dplyr::select(dplyr::any_of(select_cols))

  } else {
    stop("Only 'observation', 'duplicate' or 'aspen' are allowed as test values, enclosed in double quotes.")
  }

  if (format_table) { tbl <- format_qc(tbl, shade_by = shade_by)}

  return(tbl)
}

#' Check for trees that have not been identified
#'
#' @description
#' When a dead tree is encountered in the field, it may not be possible to identify
#' it. The column hasLiveTree in the results table is TRUE when an unidentified tree
#' has at least one live tree recorded and is FALSE otherwise.
#'
#' @param aspen_data Nested list of aspen dataframes in data package format, as returned by `load_and_package_aspen()`
#' @param format_table Logical. TRUE applies reactable table formatting, use for Rmd. Use FALSE for writing to CSV.
#'
#' @returns Table containing summary of trees that have not been identified and whether or not the record has a live tree recorded
#' @export
check_tree_id <- function(aspen_data, format_table = TRUE) {
  # Identify trees that are missing identification
  tbl <- aspen_data$data$Observations %>%
    dplyr::filter(is.na(speciesCode) |
                    is.na(verbatimIdentification) |
                    is.na(scientificName)) %>%
    dplyr::mutate(live = sizeClass %in% c("Class I", "Class II", "Class III", "Class IV", "Class V") &
                    individualCount > 0) %>%
    # Determine if these trees have live trees recorded
    dplyr::group_by(unitCode, siteID, eventDate, speciesCode, verbatimIdentification, scientificName) %>%
    dplyr::summarise(hasLiveTree = any(live),
                     .groups = "drop")

  if (format_table) { tbl <- format_qc(tbl) }

  return(tbl)
}

#' Check total tree count values
#'
#' @description
#' For a each species observed during a site visit, the total tree count is the
#' sum of individuals across all size classes (including dead). Species that have
#' a total count less than one or greater than 250 are identified during this check.
#'
#' @inheritParams check_tree_id
#'
#' @returns Table containing summary of visits and species with a total count less than one or greater than 250
#' @export
check_tree_count <- function(aspen_data, format_table = TRUE) {
  # Calculate total count by tree for each site visit
  tbl <- aspen_data$data$Observations %>%
    dplyr::group_by(siteID, eventDate, visitType, scientificName) %>%
    dplyr::summarise(totalTreeCount = sum(individualCount),
                     .groups = "drop") %>%
    # Filter to zero or >250 counts
    dplyr::filter(totalTreeCount < 1 | totalTreeCount > 250)

  if (format_table) { tbl <- format_qc(tbl) }

  return(tbl)
}

#' Check for duplicate or missing size classes
#'
#' @description
#' `check_size_class` performs two tests on the observations table. When \code{test
#' = "duplicate"} it returns duplicate size class records for a tree. When \code{test = "
#' missing"} it returns all available records for trees missing a size class observation.
#' For example, if a tree is missing a Class I record, its Class II - Class VI
#' records are returned.
#'
#' @inheritParams check_tree_id
#' @param test Character string representing what test to perform. Must be "duplicate" or "missing"
#'
#' @returns Table containing observation records that met the criteria for the specified test
#' @export
check_size_class <- function(aspen_data, test, format_table = TRUE) {
  if (test == "duplicate") {
    # Identify duplicate tree records
    tbl <- aspen_data$data$Observations %>%
      dplyr::add_count(siteID, eventDate, scientificName, visitType, sizeClass) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(unitCode, siteID, eventDate, visitType, speciesCode, scientificName, sizeClass, individualCount) %>%
      dplyr::arrange(siteID, eventDate, scientificName, sizeClass, visitType)

    shade_by <- c("siteID", "eventDate", "scientificName", "visitType", "sizeClass")

  } else if (test == "missing") {
    # Identify trees missing a size class
    tbl <- aspen_data$data$Observations %>%
      dplyr::group_by(siteID, eventDate, scientificName, visitType) %>%
      dplyr::filter(dplyr::n_distinct(sizeClass) < 6) %>%
      dplyr::ungroup() %>%
      dplyr::select(unitCode, siteID, eventDate, visitType, speciesCode,
                    scientificName, sizeClass, individualCount) %>%
      dplyr::arrange(siteID, eventDate, scientificName, sizeClass, visitType)

    shade_by <- c("siteID", "eventDate", "scientificName", "visitType")

  } else {
    stop("Only 'duplicate' or 'missing' are allowed as test values, enclosed in double quotes.")
  }

  if (format_table) { tbl <- format_qc(tbl, shade_by = shade_by)}

  return(tbl)
}

#' Check for duplicate pest records for a tree
#'
#' @inheritParams check_tree_id
#'
#'@returns Table containing duplicate pest records
#'@export
check_duplicate_pests <- function(aspen_data, format_table = TRUE) {
  select_cols <- c("unitCode", "siteID", "eventDate", "stand", "transect",
                   "plotNumber", "speciesCode", "scientificName", "pest")
  shade_by <- c("unitCode", "siteID", "eventDate","scientificName", "pest")

  tbl <- aspen_data$data$Pests %>%
    dplyr::add_count(siteID, eventDate, speciesCode, scientificName, pest) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(dplyr::any_of(select_cols)) %>%
    dplyr::arrange(siteID, eventDate, speciesCode, pest)

  if (format_table) { tbl <- format_qc(tbl, shade_by = shade_by)}

  return(tbl)
}

#' Check for duplicate disturbance records for a site visit
#'
#' @inheritParams check_tree_id
#'
#'@returns Table containing duplicate disturbance records
#'@export
check_duplicate_disturbances <- function(aspen_data, format_table = TRUE) {
  select_cols <- c("unitCode", "siteID", "eventDate", "stand", "transect",
                   "plotNumber", "disturbance")
  shade_by <- c("unitCode", "siteID", "eventDate", "disturbance")

  tbl <- aspen_data$data$Disturbances %>%
    dplyr::add_count(siteID, eventDate, disturbance) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(dplyr::any_of(select_cols)) %>%
    dplyr::arrange(siteID, eventDate, disturbance)

  if (format_table) { tbl <- format_qc(tbl, shade_by = shade_by)}

  return(tbl)
}

#' Summary count of missing values in a table
#'
#' @description
#' Provides a count of missing values for each column in a table. Cells are color
#' coded based on the proportion of rows with missing values: no color for 0% missing,
#' pale yellow for up to 25% missing, yellow for up to 50% missing, orange for up
#' to 75% missing, dark orange for greater than 75% missing, and red when all values
#' in a column are missing.
#'
#' @inheritParams check_tree_id
#' @param table Name of table to summarize. Must be "SiteVisit", "Disturbances", "Observations" or "Pests"
#'
#' @returns Table with original column names and a single row with the count of NA values in that column
#' @export
count_missing_values <- function(aspen_data, table, format_table = TRUE) {

  if (!table %in% c("SiteVisit", "Disturbances", "Observations", "Pests")) {

    stop("Please provide a valid table value: SiteVisit, Disturbances, Observations or Pests.")

  } else {

    num_rows <- nrow(aspen_data$data[[table]])

    tbl <- aspen_data$data[[table]] %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.))))
  }

  if (format_table) { tbl <- format_qc(tbl, missing_row_color = TRUE, total_rows = num_rows)}

  return(tbl)
}
