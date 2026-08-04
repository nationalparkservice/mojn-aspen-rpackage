#' @importFrom magrittr %>% %<>%
#' @import dplyr
#' @import fetchagol

pkg_globals <- new.env(parent = emptyenv())

#' Wrangle optional sites table
#'
#' @param raw_site_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Sites_Master
#'
#' @return raw_site_data with updated sites table and metadata fields
wrangleSites <- function(raw_data) {
  raw_data$data$AllSites <- raw_data$data$AllSites %>%
    # Remove trailing underscores from column names
    dplyr::rename_with(~ sub("_$", "", .x)) %>%
    dplyr::mutate(
      # Expand codes to differentiate between NA char and NA
      Shift = dplyr::case_when(
        .data$Shift == "NA" ~ "Not Shifted",
        .data$Shift == "S" ~ "South",
        .data$Shift == "N" ~ "North",
        .data$Shift == "E" ~ "East",
        .data$Shift == "W" ~ "West",
        TRUE ~ .data$Shift),
      # Format UTM coordinates using DWC column names
      VerbatimCoordinates = dplyr::case_when(
        is.na(.data$Xcoord) | is.na(.data$Ycoord) ~ NA_character_,
        .data$Park == "GRBA" ~ paste0("11N ", .data$Xcoord, "m E ", .data$Ycoord, "m N"),
        .data$Park == "PARA" ~ paste0("12N ", .data$Xcoord, "m E ", .data$Ycoord, "m N")),
      # Add geographic info columns
      VerbatimCoordinateSystem = "UTM",
      VerbatimSRS = "EPSG:4269", # code for NAD83
      GeodeticDatum = "EPSG:4326" # code for WGS84
      ) %>%
      dplyr::relocate(Shift, VerbatimCoordinateSystem, VerbatimSRS, VerbatimCoordinates, GeodeticDatum, .before = Lat) %>%
      dplyr::relocate(Stand_Height, .after = Zone) %>%
      # Remove unnecessary columns
      dplyr::select(-SiteDescription, -LegacyFrame, -NavigationNotes, -Xcoord, -Ycoord, -GlobalID)

  return(raw_data)
}

#' Wrangle site visit table
#'
#' @param raw_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated site visit table and metadata fields
wrangleSiteVisit <- function(raw_data) {
  raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
    # Expand park code
    dplyr::left_join(raw_data$metadata$SiteVisit$fields$Park$lookup$lookup_df %>%
                       dplyr::select(Park = name, ParkName = description),
                     by = dplyr::join_by(Park)) %>%
    dplyr::relocate(ParkName, .after = Park) %>%
    # Expand protocol version
    dplyr::left_join(raw_data$metadata$SiteVisit$fields$ProtocolVersion$lookup$lookup_df %>%
                       dplyr::select(ProtocolVersion = name, label),
                     by = join_by(ProtocolVersion)) %>%
    dplyr::mutate(VisitDate = as.Date(.data$VisitDate),
                  ProtocolVersion = .data$label) %>%
    # Join site info from all sites tbl
    dplyr::left_join(raw_data$data$AllSites,
                     by = dplyr::join_by(Park, Site)) %>%
    dplyr::relocate(Stratum, Zone, VisitDate, VisitType, .before = Observer) %>%
    dplyr::relocate(Evaluation:GRTSAssessment, ProtocolVersion, Community, VisitNotes, .after = dplyr::last_col()) %>%
    dplyr::select(-StartTime, -EndTime, -label, -parentglobalid)

  return(raw_data)
}

#' Wrangle disturbances table
#'
#' @param raw_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated disturbances table and metadata fields
wrangleDisturbances <- function(raw_data) {
  # Join site visit info to disturbance tbl
  raw_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, Park, ParkName, Site, VisitDate, VisitType, Community) %>%
    dplyr::right_join(raw_data$data$Disturbances,
                      by = dplyr::join_by("parentglobalid")) %>%
    dplyr::rename(DisturbanceCode = Disturbance) %>%
    # Expand disturbance codes
    dplyr::left_join(raw_data$metadata$Disturbances$fields$Disturbance$lookup$lookup_df %>%
                       dplyr::select(DisturbanceCode = name, Disturbance = label),
                     by = join_by(DisturbanceCode)) %>%
    dplyr::select(-parentglobalid, -globalid, -DisturbanceCode)

  return(raw_data)
}

#' Wrangle observations table
#'
#' @param raw_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated observations table and metadata fields
wrangleObservations <- function(raw_data) {
  # Join site visit info to observations tbl
  raw_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, Park, ParkName, Site, VisitDate, VisitType, Community) %>%
    dplyr::right_join(raw_data$data$Observations,
                      by = dplyr::join_by("parentglobalid")) %>%
    # Expand species codes to show full scientific names
    dplyr::left_join(raw_data$metadata$Observations$fields$SpeciesCode$lookup$lookup_df %>%
                       dplyr::mutate(SpeciesName = gsub("\\s*\\([^)]*\\)", "", label)),
                     by = join_by(SpeciesCode == name)) %>%
    dplyr::relocate(SpeciesName, .after = SpeciesCode) %>%
    # Pivot to tidy format
    tidyr::pivot_longer(cols = dplyr::contains("Class"),
                        names_to = "SizeClass",
                        values_to = "IndividualCount") %>%
    dplyr::mutate(
      # Display class sizes as roman numerals to match UCBN data and protocol
      SizeClass = dplyr::case_when(
        .data$SizeClass == "Class1" ~ "Class I",
        .data$SizeClass == "Class2" ~ "Class II",
        .data$SizeClass == "Class3" ~ "Class III",
        .data$SizeClass == "Class4" ~ "Class IV",
        .data$SizeClass == "Class5" ~ "Class V",
        .data$SizeClass == "Class6" ~ "Class VI"
        ),
      # Add class descriptions
      SizeClassDescription = dplyr::case_when(
        .data$SizeClass == "Class I" ~ "Suckers or seedlings less than 46 cm tall",
        .data$SizeClass == "Class II" ~ "Suckers or seedlings 46 cm to 152 cm tall",
        .data$SizeClass == "Class III" ~ "Greater than 152 cm and up to 2.5 cm in dbh",
        .data$SizeClass == "Class IV" ~ "Greater than 2.5 cm in dbh and shorter than 75% of the stand height",
        .data$SizeClass == "Class V" ~ "Greater than 2.5 cm in dbh and taller than 75% of the stand height",
        .data$SizeClass == "Class VI" ~ "Dead stems greater than 2.5 cm in dbh"
        )) %>%
    dplyr::select(-parentglobalid, -label)

  # Remove ID col from SiteVisit tbl, not needed after join
  raw_data$data$SiteVisit["globalid"] <- NULL

  return(raw_data)
}

#' Wrangle pests table
#'
#' @param raw_data Object returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated pests table and metadata fields
wranglePests <- function(raw_data) {
  # Join site visit and species info from observations tbl to pests
  raw_data$data$Pests <- raw_data$data$Observations %>%
    dplyr::select(parentglobalid = globalid, Park, ParkName, Site, VisitDate, VisitType, Community, SpeciesCode, SpeciesName) %>%
    dplyr::distinct() %>%
    dplyr::right_join(raw_data$data$Pests,
                      by = join_by("parentglobalid")) %>%
    dplyr::rename(PestCodes = Pest) %>%
    # Expand out shortened pest names
    dplyr::left_join(raw_data$metadata$Pests$fields$Pest$lookup$lookup_df %>%
                       dplyr::select(PestCodes = name, Pest = label),
                     by = join_by(PestCodes)) %>%
    dplyr::select(-parentglobalid, -globalid, -PestCodes)

  # Remove ID col from Observations tbl, not needed after join
  raw_data$data$Observations["globalid"] <- NULL

  return(raw_data)
}

#' Format aspen data for data package publication
#'
#' @param aspen_data The wrangled aspen data
#'
#' @return aspen_data with data package formatting
packageMOJNAspen <- function(aspen_data) {
  # Remove metadata and AllSites tbl from final data package tbls
  aspen_data$metadata <- NULL
  aspen_data$data <- aspen_data$data[names(aspen_data$data) != "AllSites"]

  tbl_names <- names(aspen_data$data)

  aspen_data$data <- lapply(tbl_names, function(nm) {
    tbl <- aspen_data$data[[nm]]
    # Wrangling for all tbls
    tbl <- tbl %>%
      janitor::clean_names(case = "lower_camel") %>%
      dplyr::rename(unitCode = park, # CSO standard
                    unitName = parkName, # CSO standard
                    siteID = site, # CSO standard
                    eventDate = visitDate # DWC name
                    ) %>%
      dplyr::mutate(type = "Event", # DWC column
                    basisOfRecord = "HumanObservation" # DWC column
                    ) %>%
      dplyr::relocate(unitName, .after = unitCode)

    # Update taxonomy
    if("speciesName" %in% names(tbl)) {
      tbl <- tbl %>%
        dplyr::rename(verbatimIdentification = speciesName) %>%
        dplyr::mutate(scientificName = dplyr::na_if(.data$verbatimIdentification, "Unknown")) %>%
        QCkit::get_taxon_rank("scientificName") %>%
        dplyr::relocate(scientificName, taxonRank, .after = verbatimIdentification)

      # Hard code temporary fixes
      tbl$scientificName[tbl$scientificName == "Cercocarpus ledifollius"] <- "Cercocarpus ledifolius"
      tbl$scientificName[tbl$scientificName == "Pinus longeava"] <- "Pinus longaeva"
      }

    # Update site visit tbl
    if(nm == "SiteVisit") {
      tbl <- tbl %>%
        dplyr::rename(verbatimSRS = verbatimSrs, # DWC name
                      decimalLatitude = lat, # DWC name
                      decimalLongitude = long, # DWC name
                      GRTSAssessment = grtsAssessment,
                      elevationInMeters = elevation, # Add unit to col name
                      slopeInPercent = slope, # Add unit to col name
                      aspectInDegrees = aspect, # Add unit to col name
                      GRTSOrder = grtsOrder) %>%
        dplyr::mutate(basisOfRecord = "Event")
      }
    return(tbl)
    })
  names(aspen_data$data) <- tbl_names
  return(aspen_data)
}

#' Fetch, wrangle, and package aspen data from AGOL
#'
#' @param aspen_url URL to MOJN_Aspen_Database on AGOL
#' @param site_url URL to MOJN_Aspen_Sites_Master on AGOL
#' @param agol_username AGOL headless account username
#' @param agol_password AGOL headless account password (do not hard code this into your scripts!)
#'
#' @return A list of aspen data frames in data package format
#' @export
loadAndWrangleMOJNAspen <- function(
    aspen_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Aspen_Test_Visit_NonSpatial_gdb/FeatureServer",
    site_url =  "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/AspenSites2/FeatureServer",
    agol_username = "mojn_data",
    agol_password = keyring::key_get(service = "AGOL", username = agol_username)) {

  # Import aspen db and all sites tbl
  raw_data <- fetchagol::fetchRawData(aspen_url, agol_username, agol_password)
  raw_site_data <- fetchagol::fetchRawData(site_url, agol_username, agol_password)

  # Combine for processing
  raw_data$data$AllSites <- raw_site_data$data$`MOJN Aspen Sites Master`
  raw_data$metadata$AllSites <- raw_site_data$metadata$`MOJN Aspen Sites Master`

  # Wrangle all tbls
  aspen_data <- raw_data %>%
    fetchagol::cleanData(cols_to_remove =
                           grep("Edit|Creat|DataProcessing", unique(unlist(lapply(raw_data$data, names))), value = TRUE)) %>%
    wrangleSites() %>%
    wrangleSiteVisit() %>%
    wrangleDisturbances() %>%
    wrangleObservations() %>%
    wranglePests() %>%
    packageMOJNAspen()

  # Store imported data as a global variable so that all package functions can access it without the user having to pass the dataset as an argument
  assign("mojn_aspen_data", aspen_data, envir = pkg_globals)
  invisible(aspen_data)
}
