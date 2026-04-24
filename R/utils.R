#' @importFrom magrittr %>% %<>%
#' @import dplyr
#' @import fetchagol

pkg_globals <- new.env(parent = emptyenv())

#' Update metadata fields to match column names in corresponding table
#'
#' @param raw_data The raw_data object as returned by fetchagol::fetchRawData
#'
#' @return raw_data with updated metadata fields
.update_metadata_fields <- function(raw_data) {
  for (tbl in names(raw_data$data)) {
    cols <- names(raw_data$data[[tbl]])
    fields <- raw_data$metadata[[tbl]]$fields

    # Identify columns missing in metadata
    add_cols <- setdiff(cols, names(fields))

    # Create minimal metadata for missing columns
    if (length(add_cols)) {
      fields[add_cols] <- stats::setNames(
        lapply(add_cols, function(colname) {
          col_class <- class(raw_data$data[[tbl]][[colname]])
          list(description = colname,
               attributes  = list(class = col_class))
        }),
        add_cols
      )
    }
    # Reorder metadata fields to match the table's column order
    raw_data$metadata[[tbl]]$fields <-
      if (length(cols)) fields[cols] else list()
  }
  invisible(raw_data)
}

#' Wrangle optional sites table
#'
#' @param raw_site_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Sites_Master
#'
#' @return raw_site_data with updated sites table and metadata fields
wrangleAllSites <- function(raw_site_data) {
  raw_site_data$metadata$`MOJN Aspen Sites Master`$table_name <- "AllSites"

  # Remove trailing underscores from column names
  names(raw_site_data$data$`MOJN Aspen Sites Master`) <- sub("_$", "", names(raw_site_data$data$`MOJN Aspen Sites Master`))
  names(raw_site_data$metadata$`MOJN Aspen Sites Master`$fields) <- sub("_$", "", names(raw_site_data$metadata$`MOJN Aspen Sites Master`$fields))

  raw_site_data$data$`MOJN Aspen Sites Master` <- raw_site_data$data$`MOJN Aspen Sites Master` %>%
    dplyr::mutate(
      # Expand codes to differentiate between NA char and NA
      Shift = dplyr::case_when(
        Shift == "NA" ~ "Not Shifted",
        Shift == "S" ~ "South",
        Shift == "N" ~ "North",
        Shift == "E" ~ "East",
        Shift == "W" ~ "West",
        TRUE ~ Shift),
      # Format UTM coordinates
      VerbatimCoordinates = dplyr::case_when(
        is.na(Xcoord) | is.na(Ycoord) ~ NA_character_,
        Park == "GRBA" ~ paste0("11N ", Xcoord, "m E ", Ycoord, "m N"),
        Park == "PARA" ~ paste0("12N ", Xcoord, "m E ", Ycoord, "m N")),
      # Add geographic info columns
      VerbatimCoordinateSystem = "UTM",
      VerbatimSRS = "EPSG:4269", # code for NAD 83
      GeodeticDatum = "EPSG:4326" # code for WGS 84 (for lat/lon coordinates)
    ) %>%
    dplyr::relocate(VerbatimCoordinateSystem, VerbatimSRS, VerbatimCoordinates, GeodeticDatum, .before = Lat) %>%
    dplyr::relocate(Community, Stand_Height, .after = Zone) %>%
    # Remove unnecessary columns
    dplyr::select(-"SiteDescription", -"LegacyFrame", -"NavigationNotes", -"Xcoord", -"Ycoord")

  # Add new columns to metadata fields
  raw_site_data <- .update_metadata_fields(raw_site_data)

  invisible(raw_site_data)
}

#' Fetch aspen data from AGOL and do preliminary data wrangling
#'
#' @param aspen_url URL to main AGOL aspen database
#' @param site_url URL to AGOL database for aspen sites (if applicable)
#' @param agol_username Authentication token (not needed for public layers)
#'
#' @return A list of data frames and metadata
#' @export

# TODO: add variables that were joined to data tables to each metadata table
loadAndWrangleMOJNAspen <- function(
    aspen_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Aspen_Test_Visit_NonSpatial_gdb/FeatureServer",
                                 site_url =  "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/AspenSites2/FeatureServer",
                                 agol_username = "mojn_data") {
  flattened_data <- list(data = list(),
                         metadata = list())

  # Import aspen database
  raw_data <- fetchagol::fetchRawData(aspen_url, agol_username)


  # Imports optional second database and connects it to main database
  # For MOJN - there's a master list of sites in a different database
  if(!is.null(site_url)) {
    # Import aspen site database
    raw_site_data <- fetchagol::fetchRawData(site_url, agol_username)

    # Add site data to list of other data
    raw_data$data$AllSites <- raw_site_data$data$`MOJN Aspen Sites Master`
    # Join site metadata to list of other metadata
    raw_data$metadata$AllSites <- raw_site_data$metadata$`MOJN Aspen Sites Master`
  }

  raw_data <- fetchagol::cleanData(raw_data)


  flattened_data$metadata <- raw_data$metadata

  # Add optional second database to flattened data
  if(!is.null(site_url)) {
  flattened_data$data$AllSites <- raw_data$data$AllSites

  # Join background site information with other tables
  raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
    dplyr::left_join(dplyr::select(flattened_data$data$AllSites, dplyr::any_of(c("Site", "Status", "Panel", "Stratum", "Zone_", "Community"))),
                     by = dplyr::join_by("Site"))
  flattened_data$metadata$SiteVisit$fields <- append(flattened_data$metadata$SiteVisit$fields, flattened_data$metadata$AllSites$fields[c("Status", "Panel", "Stratum", "Zone_", "Community")])
  flattened_data$metadata$AllSites$table_name <- "AllSites"

  }


  # Join background site information with other tables
  # flattened_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
  #   dplyr::left_join(dplyr::select(flattened_data$data$AllSites, dplyr::any_of(c("Site", "Status", "Panel", "Stratum", "Zone_", "Community"))),
  #                    by = dplyr::join_by("Site"))
  flattened_data$data$SiteVisit <- raw_data$data$SiteVisit

  flattened_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason) %>%
    dplyr::right_join(raw_data$data$Disturbances, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Disturbances$fields <- append(flattened_data$metadata$Disturbances$fields, flattened_data$metadata$SiteVisit$fields[c("Park", "VisitType", "VisitDate", "FieldSeason")])


  flattened_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason) %>%
    dplyr::right_join(raw_data$data$Observations, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Observations$fields <- append(flattened_data$metadata$Observations$fields, flattened_data$metadata$SiteVisit$fields[c("Park", "Site", "VisitType", "VisitDate", "FieldSeason")])

  flattened_data$data$Pests <- flattened_data$data$Observations %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason, SpeciesCode) %>%
    dplyr::right_join(raw_data$data$Pests, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Pests$fields <- append(flattened_data$metadata$Pests$fields, flattened_data$metadata$Observations$fields[c("Park", "VisitType", "VisitDate", "FieldSeason", "SpeciesCode")])

  invisible(flattened_data)
}

#' Fetch aspen data from AGOL and do preliminary data wrangling
#'
#' @param aspen_url URL to main AGOL aspen database
#' @param site_url URL to AGOL database for aspen sites (if applicable)
#' @param agol_username Authentication token (not needed for public layers)
#'
#' @return A list of data frames and metadata
#' @export

loadAndWrangleUCBNAspen <- function(
    aspen_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/service_4d6343e9204142928351c52c6f1362c5/FeatureServer",
                                 site_url =  NULL,
                                 agol_username = "mojn_data") {
  flattened_data <- list(data = list(),
                         metadata = list())

  # Import aspen database
  raw_data <- fetchagol::fetchRawData(aspen_url, agol_username)


  # Imports optional second database and connects it to main database
  # For MOJN - there's a master list of sites in a different database
  if(!is.null(site_url)) {
    # Import aspen site database
    raw_site_data <- fetchagol::fetchRawData(site_url, agol_username)

    # Add site data to list of other data
    raw_data$data$AllSites <- raw_site_data$data$`MOJN Aspen Sites Master`
    # Join site metadata to list of other metadata
    raw_data$metadata$AllSites <- raw_site_data$metadata$`MOJN Aspen Sites Master`
  }

  raw_data <- fetchagol::cleanData(raw_data)

  # Add optional second database to flattened data
  if(!is.null(site_url)) {
    flattened_data$data$AllSites <- raw_data$data$AllSites

    flattened_data$metadata <- raw_data$metadata

    # Join background site information with other tables and add background information metadata to tables it was added to
    raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
      dplyr::left_join(dplyr::select(flattened_data$data$AllSites, dplyr::any_of(c("Site", "Status", "Panel", "Stratum", "Zone_", "Community"))),
                       by = dplyr::join_by("Site"))}
  flattened_data$metadata$SiteVisit$fields <- append(flattened_data$metadata$SiteVisit$fields, flattened_data$metadata$AllSites$fields[c("Site", "Status", "Panel", "Stratum", "Zone_", "Community")])


  # Join background site information with other tables
  # flattened_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
  #   dplyr::left_join(dplyr::select(flattened_data$data$AllSites, dplyr::any_of(c("Site", "Status", "Panel", "Stratum", "Zone_", "Community"))),
  #                    by = dplyr::join_by("Site"))
  flattened_data$data$SiteVisit <- raw_data$data$SiteVisit

  flattened_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Stand, Transect, VisitDate, PlotNum, Unique_ID) %>%
    dplyr::right_join(raw_data$data$Disturbances, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Disturbances$fields <- append(flattened_data$metadata$Disturbances$fields, flattened_data$metadata$SiteVisit$fields[c("Park", "Stand", "Transect", "VisitDate", "PlotNum", "Unique_ID")])

  flattened_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Stand, Transect, VisitDate, PlotNum, Unique_ID) %>%
    dplyr::right_join(raw_data$data$Observations, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Observations$fields <- append(flattened_data$metadata$Observations$fields, flattened_data$metadata$SiteVisit$fields[c("Park", "Stand", "Transect", "VisitDate", "PlotNum", "Unique_ID")])

  flattened_data$data$Pests <- flattened_data$data$Observations %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Stand, Transect, VisitDate, PlotNum, Unique_ID) %>%
    dplyr::right_join(raw_data$data$Pests, by = c("parentglobalid" = "parentglobalid"))
  flattened_data$metadata$Pests$fields <- append(flattened_data$metadata$Pests$fields, flattened_data$metadata$SiteVisit$fields[c("Park", "Stand", "Transect", "VisitDate", "PlotNum", "Unique_ID")])

  invisible(flattened_data)
}

#' Write aspen data to CSV
#'
#' @inheritParams fetchagol::writeToFiles
#'
#' @export
#'
writeAspen <- function(all_data, data_dir = here::here("data", "final"), dictionary_dir = here::here("data", "dictionary"),
                      dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                               attributes = "data_dictionary_attributes.txt",
                                               categories = "data_dictionary_categories.txt"),
                      verbose = FALSE, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"))
{
  fetchagol::writeToFiles(all_data = all_data, data_dir = data_dir, dictionary_dir = dictionary_dir, lookup_dir = NA, verbose = verbose, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"))
}



# TODO: should a get_data() function be added like in pine??






