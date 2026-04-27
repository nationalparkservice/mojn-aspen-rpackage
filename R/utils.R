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
      GeodeticDatum = "EPSG:4326" # code for WGS 84
      ) %>%
      dplyr::relocate(VerbatimCoordinateSystem, VerbatimSRS, VerbatimCoordinates, GeodeticDatum, .before = Lat) %>%
      dplyr::relocate(Community, Stand_Height, .after = Zone) %>%
      # Remove unnecessary columns
      dplyr::select(-dplyr::any_of(c("SiteDescription", "LegacyFrame", "NavigationNotes","Xcoord", "Ycoord", "GlobalID")))

  # Add new columns to metadata fields
  raw_site_data <- .update_metadata_fields(raw_site_data)

  invisible(raw_site_data)
}

#' Wrangle site visit table
#'
#' @inheritParams .update_metadata_fields
#'
#' @return raw_data with updated site visit table and metadata fields
wrangleSiteVisit <- function(raw_data) {
  raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
    # Expand protocol version
    dplyr::left_join(raw_data$metadata$SiteVisit$fields$ProtocolVersion$lookup$lookup_df %>%
                       dplyr::select(ProtocolVersion = name, label),
                     by = join_by(ProtocolVersion)) %>%
    dplyr::mutate(VisitDate = as.Date(VisitDate),
                  ProtocolVersion = label) %>%
    dplyr::relocate(Observer:Recorder, ProtocolVersion, FieldSeason, .after = VisitDate) %>%
    dplyr::select(-StartTime, -EndTime, -label, -parentglobalid)

  # If sites tbl is available, join on sites info
  if(!is.null(raw_data$data$AllSites)) {
    raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
      dplyr::left_join(dplyr::select(raw_data$data$AllSites,
                                     dplyr::any_of(c("Site", "Status", "Stratum", "Zone", "Community"))),
                       by = dplyr::join_by(Site)) %>%
      dplyr::relocate(Stratum:Zone, .after = Site) %>%
      dplyr::relocate(VisitNotes, .after = dplyr::last_col())
  }

  # Add new columns to metadata fields
  raw_data <- .update_metadata_fields(raw_data)

  invisible(raw_data)
}

#' Wrangle disturbances table
#'
#' @inheritParams .update_metadata_fields
#'
#' @return raw_data with updated disturbances table and metadata fields
wrangleDisturbances <- function(raw_data) {
  # Join site visit info to disturbance tbl
  raw_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, Park, Site, VisitType, VisitDate, dplyr::any_of("Community")) %>%
    dplyr::right_join(raw_data$data$Disturbances,
                      by = dplyr::join_by("parentglobalid")) %>%
    dplyr::rename(DisturbanceCode = Disturbance) %>%
    # Expand disturbance codes
    dplyr::left_join(raw_data$metadata$Disturbances$fields$Disturbance$lookup$lookup_df %>%
                       dplyr::select(DisturbanceCode = name, Disturbance = label),
                     by = join_by(DisturbanceCode)) %>%
    dplyr::select(-parentglobalid, -globalid, -DisturbanceCode)

  # Add new columns to metadata fields
  raw_data <- .update_metadata_fields(raw_data)

  invisible(raw_data)
}

#' Wrangle observations table
#'
#' @inheritParams .update_metadata_fields
#'
#' @return raw_data with updated observations table and metadata fields
wrangleObservations <- function(raw_data) {
  # Join site visit info to observations tbl
  raw_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, Park, Site, VisitType, VisitDate, dplyr::any_of("Community")) %>%
    dplyr::right_join(raw_data$data$Observations,
                      by = dplyr::join_by("parentglobalid")) %>%
    # Expand species codes to show full scientific names
    dplyr::left_join(raw_data$metadata$Observations$fields$SpeciesCode$lookup$lookup_df %>%
                       dplyr::mutate(ScientificName = gsub("\\s*\\([^)]*\\)", "", label)),
                     by = join_by(SpeciesCode == name)) %>%
    dplyr::relocate(ScientificName, .after = SpeciesCode) %>%
    # Pivot to tidy format
    tidyr::pivot_longer(cols = dplyr::contains("Class"),
                        names_to = "SizeClass",
                        values_to = "TreeCount") %>%
    # Add class descriptions
    dplyr::mutate(SizeClassDescription = dplyr::case_when(
      SizeClass == "Class1" ~ "Suckers or seedlings less than 46 cm tall",
      SizeClass == "Class2" ~ "Suckers or seedlings l46 cm to 152 cm tall",
      SizeClass == "Class3" ~ "Greater than 152 cm and up to 2.5 cm in dbh",
      SizeClass == "Class4" ~ "Greater than 2.5 cm in dbh and shorter than 75% of the stand height",
      SizeClass == "Class5" ~ "Greater than 2.5 cm in dbh and taller than 75% of the stand height",
      SizeClass == "Class6" ~ "Dead stems greater than 2.5 cm in dbh",
    )) %>%
    dplyr::select(-parentglobalid, -label)

  # Add new columns to metadata fields
  raw_data <- .update_metadata_fields(raw_data)

  invisible(raw_data)
}

#' Wrangle pests table
#'
#' @inheritParams .update_metadata_fields
#'
#' @return raw_data with updated pests table and metadata fields
wranglePests <- function(raw_data) {
  # Join site visit and species info from observations tbl to pests
  raw_data$data$Pests <- raw_data$data$Observations %>%
    dplyr::select(parentglobalid = globalid, Park, Site, VisitType, VisitDate, dplyr::any_of("Community"), SpeciesCode, ScientificName) %>%
    dplyr::distinct() %>%
    dplyr::right_join(raw_data$data$Pests,
                      by = join_by("parentglobalid")) %>%
    dplyr::rename(PestCodes = Pest) %>%
    # Expand out shortened pest names
    dplyr::left_join(raw_data$metadata$Pests$fields$Pest$lookup$lookup_df %>%
                       dplyr::select(PestCodes = name, Pest = label),
                     by = join_by(PestCodes)) %>%
    dplyr::select(-parentglobalid, -globalid, -PestCodes)

  # Add new columns to metadata fields
  raw_data <- .update_metadata_fields(raw_data)

  invisible(raw_data)
}

#' Fetch aspen data from AGOL and perform preliminary data wrangling
#'
#' @param aspen_url URL to main AGOL aspen database
#' @param site_url URL to AGOL database for aspen sites (if applicable)
#' @param agol_username Username to AGOL account to access internal data
#'
#' @return A list of data frames and metadata
#' @export
loadAndWrangleMOJNAspen <- function(
    aspen_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Aspen_Test_Visit_NonSpatial_gdb/FeatureServer",
    site_url =  "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/AspenSites2/FeatureServer",
    agol_username = "mojn_data") {

  # Import aspen db
  raw_data <- fetchagol::fetchRawData(aspen_url, agol_username)

  # If sites url is provided, load and wrangle sites data
  if(!is.null(site_url)) {
    raw_site_data <- fetchagol::fetchRawData(site_url, agol_username) %>%
      wrangleAllSites()
    # Add to raw_data
    raw_data$data$AllSites <- raw_site_data$data$`MOJN Aspen Sites Master`
    raw_data$metadata$AllSites <- raw_site_data$metadata$`MOJN Aspen Sites Master`
  }

  # Remove db cols, trim white space, makes blanks NA
  cols_to_remove <- grep("Edit|Creat|DataProcessing", unique(unlist(lapply(raw_data$data, names))), value = TRUE)
  raw_data <- fetchagol::cleanData(raw_data, cols_to_remove = cols_to_remove)

  # Wrangle other tables
  aspen_data <- raw_data %>%
    wrangleSiteVisit() %>%
    wrangleDisturbances() %>%
    wrangleObservations() %>%
    wranglePests()

  # Remove ID cols that were retained for joins from data and metadata
  for (tbl in c("SiteVisit", "Observations")) {
    aspen_data$data[[tbl]]["globalid"] <- NULL
    aspen_data$metadata[[tbl]]$fields["globalid"] <- NULL
  }

  invisible(aspen_data)
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






