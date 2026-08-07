#' @importFrom magrittr %>% %<>%
#' @import dplyr
#' @import fetchagol

# Initiate new environment accessible from within package
pkg_globals <- new.env(parent = emptyenv())

# Assign global variables to avoid the "no visible binding for global variable 'x'" error in build checks
globalVariables(c(
  "Community", "Disturbance", "disturbanceCode", "EndTime", "Evaluation",
  "GRTSAssessment", "GeodeticDatum", "GlobalID", "Lat", "LegacyFrame",
  "NavigationNotes", "Observer", "Park", "ParkName", "Pest", "pestCodes",
  "ProtocolVersion", "Shift", "Site", "SiteDescription", "SpeciesCode",
  "speciesName", "Stand_Height", "StartTime", "Stratum", "Recorder",
  "VerbatimCoordinateSystem", "VerbatimCoordinates", "VerbatimSRS",
  "verbatimCoordinateSystem", "verbatimCoordinates", "verbatimSRS",
  "VisitDate", "VisitNotes", "VisitType", "Xcoord", "Ycoord", "Zone",
  "aspect", "description","parentglobalid", "elevation", "globalid",
  "grtsAssessment", "grtsOrder", "label", "lat", "Long", "name", "park",
  "parkName", "scientificName", "site", "slope",
  "taxonRank",  "unitCode", "unitName", "verbatimIdentification",
  "verbatimSrs", "eventDate", "FieldSeason", "siteID", "geodeticDatum",
  "protocolVersion", "shift", "Aspect", "aspectInDegrees", "Coord_Syst",
  "Elevation", "Loc_Name", "PlotNum", "Site_Height", "Slope", "Stand",
  "Transect", "UTM_Zone", "Unique_ID", "decimalLatitude", "decimalLongitude",
  "UnitCode", "cntClass6List", "sppSummaryCode"
                ))

#' Wrangle sites table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on MOJN_Aspen_Sites_Master
#'
#' @return raw_data with updated sites table
wrangleSites <- function(raw_data) {
  raw_data$data$AllSites <- raw_data$data$AllSites %>%
    # Remove trailing underscores from column names
    dplyr::rename_with(~ sub("_$", "", .x)) %>%
    dplyr::mutate(
      # Expand codes to differentiate between NA char and NA
      shift = dplyr::case_when(
        Shift == "NA" ~ "Not Shifted",
        Shift == "S" ~ "South",
        Shift == "N" ~ "North",
        Shift == "E" ~ "East",
        Shift == "W" ~ "West",
        TRUE ~ Shift),
      # Format UTM coordinates using DWC column names
      verbatimCoordinates = dplyr::case_when(
        is.na(Xcoord) | is.na(Ycoord) ~ NA_character_,
        Park == "GRBA" ~ paste0("11N ", Xcoord, "m E ", Ycoord, "m N"),
        Park == "PARA" ~ paste0("12N ", Xcoord, "m E ", Ycoord, "m N")),
      # Add geographic info columns
      verbatimCoordinateSystem = "UTM",
      verbatimSRS = "EPSG:4269", # code for NAD83
      geodeticDatum = "EPSG:4326" # code for WGS84
      ) %>%
    dplyr::relocate(shift, verbatimCoordinateSystem, verbatimSRS, verbatimCoordinates, geodeticDatum, .before = Lat) %>%
    dplyr::relocate(Stand_Height, .after = Zone) %>%
    dplyr::rename(
      # DWC names
      decimalLatitude = Lat,
      decimalLongitude = Long,
      # Add units to column names
      elevationInMeters = Elevation,
      slopeInPercent = Slope,
      aspectInDegrees = Aspect) %>%
    # Remove unnecessary columns
    dplyr::select(-SiteDescription, -LegacyFrame, -NavigationNotes, -Xcoord, -Ycoord, -GlobalID, -Shift)

  return(raw_data)
}

#' Wrangle site visit table
#'
#' @param raw_data Data returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated site visit table
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
    dplyr::mutate(eventDate = as.Date(VisitDate),
                  protocolVersion = label) %>%
    # Join site info from all sites tbl
    dplyr::left_join(raw_data$data$AllSites,
                     by = dplyr::join_by(Park, Site)) %>%
    dplyr::relocate(Stratum, Zone, eventDate, VisitType, FieldSeason, .before = Observer) %>%
    dplyr::relocate(Evaluation:GRTSAssessment, protocolVersion, Community, VisitNotes, .after = dplyr::last_col()) %>%
    dplyr::rename(
      # CSO standard column names
      unitCode = Park,
      unitName = ParkName,
      siteID = Site,
      # DWC name
      recordedBy = Recorder) %>%
    dplyr::select(-StartTime, -EndTime, -label, -parentglobalid, -VisitDate, -ProtocolVersion)

  return(raw_data)
}

#' Wrangle disturbances table
#'
#' @param raw_data Data returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated disturbances table
wrangleDisturbances <- function(raw_data) {
  # Join site visit info to disturbance tbl
  raw_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, unitCode, unitName, siteID, eventDate, VisitType, Community) %>%
    dplyr::right_join(raw_data$data$Disturbances,
                      by = dplyr::join_by("parentglobalid")) %>%
    dplyr::rename(disturbanceCode = Disturbance) %>%
    # Expand disturbance codes
    dplyr::left_join(raw_data$metadata$Disturbances$fields$Disturbance$lookup$lookup_df %>%
                       dplyr::select(disturbanceCode = name, disturbance = label),
                     by = join_by(disturbanceCode)) %>%
    dplyr::select(-parentglobalid, -globalid, -disturbanceCode)

  return(raw_data)
}

#' Wrangle observations table
#'
#' @param raw_data Data returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated observations table
wrangleObservations <- function(raw_data) {
  # Join site visit info to observations tbl
  raw_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, unitCode, unitName, siteID, eventDate, VisitType, Community) %>%
    dplyr::right_join(raw_data$data$Observations,
                      by = dplyr::join_by("parentglobalid")) %>%
    # Expand species codes to show full scientific names
    dplyr::left_join(raw_data$metadata$Observations$fields$SpeciesCode$lookup$lookup_df %>%
                       dplyr::mutate(speciesName = gsub("\\s*\\([^)]*\\)", "", label)),
                     by = join_by(SpeciesCode == name)) %>%
    dplyr::relocate(speciesName, .after = SpeciesCode) %>%
    # Pivot to tidy format
    tidyr::pivot_longer(cols = dplyr::contains("class"),
                        names_to = "sizeClass",
                        values_to = "individualCount") %>%
    dplyr::mutate(
      # Display class sizes as roman numerals to match UCBN data and protocol
      sizeClass = dplyr::case_when(
        sizeClass == "Class1" ~ "Class I",
        sizeClass == "Class2" ~ "Class II",
        sizeClass == "Class3" ~ "Class III",
        sizeClass == "Class4" ~ "Class IV",
        sizeClass == "Class5" ~ "Class V",
        sizeClass == "Class6" ~ "Class VI"
        ),
      # Add class descriptions
      sizeClassDescription = dplyr::case_when(
        sizeClass == "Class I" ~ "Suckers or seedlings less than 46 cm tall",
        sizeClass == "Class II" ~ "Suckers or seedlings 46 cm to 152 cm tall",
        sizeClass == "Class III" ~ "Greater than 152 cm and up to 2.5 cm in dbh",
        sizeClass == "Class IV" ~ "Greater than 2.5 cm in dbh and shorter than 75% of the stand height",
        sizeClass == "Class V" ~ "Greater than 2.5 cm in dbh and taller than 75% of the stand height",
        sizeClass == "Class VI" ~ "Dead stems greater than 2.5 cm in dbh"
        )) %>%
    dplyr::select(-parentglobalid, -label)

  # Remove ID col from SiteVisit tbl, not needed after join
  raw_data$data$SiteVisit["globalid"] <- NULL

  return(raw_data)
}

#' Wrangle pests table
#'
#' @param raw_data Data returned by fetchagol::fetchRawData on MOJN_Aspen_Database
#'
#' @return raw_data with updated pests table
wranglePests <- function(raw_data) {
  # Join site visit and species info from observations tbl to pests
  raw_data$data$Pests <- raw_data$data$Observations %>%
    dplyr::select(parentglobalid = globalid, unitCode, unitName, siteID, eventDate, VisitType, Community, SpeciesCode, speciesName) %>%
    dplyr::distinct() %>%
    dplyr::right_join(raw_data$data$Pests,
                      by = join_by("parentglobalid")) %>%
    dplyr::rename(pestCodes = Pest) %>%
    # Expand out shortened pest names
    dplyr::left_join(raw_data$metadata$Pests$fields$Pest$lookup$lookup_df %>%
                       dplyr::select(pestCodes = name, pest = label),
                     by = join_by(pestCodes)) %>%
    dplyr::select(-parentglobalid, -globalid, -pestCodes)

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
      # Fix acronym capitalization
      dplyr::rename(dplyr::any_of(c(siteID = "siteId",
                                    verbatimSRS = "verbatimSrs",
                                    GRTSAssessment = "grtsAssessment",
                                    GRTSOrder = "grtsOrder"))) %>%
      dplyr::mutate(
        # DWC columns
        type = "Event",
        basisOfRecord = ifelse(nm == "SiteVisit", "Event", "HumanObservation")
        )

    # Update taxonomy
    if("speciesName" %in% names(tbl)) {
      tbl <- tbl %>%
        dplyr::rename(verbatimIdentification = speciesName) %>%
        dplyr::mutate(scientificName = dplyr::na_if(verbatimIdentification, "Unknown")) %>%
        QCkit::get_taxon_rank("scientificName") %>%
        dplyr::relocate(scientificName, taxonRank, .after = verbatimIdentification)

      # Hard code temporary fixes
      tbl$scientificName[tbl$scientificName == "Cercocarpus ledifollius"] <- "Cercocarpus ledifolius"
      tbl$scientificName[tbl$scientificName == "Pinus longeava"] <- "Pinus longaeva"
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

#' Wrangle UCBN sites table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on UCBN_Aspen_Locations_pt_20210430
#'
#' @return raw_data with updated sites table
wrangleUCBNSites <- function(raw_data) {
  raw_data$data$Locations <- raw_data$data$Locations %>%
    # Generate lat/long coordinates from UTM
    QCkit::generate_ll_from_utm(EastingCol = "X_Coord", NorthingCol = "Y_Coord", ZoneCol = "UTM_Zone", DatumCol = "Datum", latlong_datum = "WGS84") %>%
    dplyr::mutate(
      # Format UTM coordinates using DWC column names
      verbatimCoordinates = ifelse(is.na(.data$X_Coord) | is.na(.data$Y_Coord), NA_character_, paste0(UTM_Zone, " ", .data$X_Coord, "m E ", .data$Y_Coord, "m N")),
      # Add geographic info columns
      verbatimSRS = "EPSG:4269", # code for NAD83
      geodeticDatum = "EPSG:4326", # code for WGS84
      # Add wkid column for lat long coordinates for use in generating elevation, aspect, slope
      # From ESRI Developer: 4326 is generally assumed to be the spatial reference when talking about "latitude" or "longitude"
      # (https://developers.arcgis.com/documentation/spatial-references/)
      latlong_wkid = "4326"
    ) %>%
    # Get elevation, aspect, slope from AGOL
    ucbn::FetchElevationAspectSlope(lat_col = "decimalLatitude", long_col =  "decimalLongitude", wkid_col = "latlong_wkid", agol_username = "mojn_data") %>%
    dplyr::select(Loc_Name, verbatimCoordinateSystem = Coord_Syst, verbatimSRS, verbatimCoordinates, geodeticDatum, decimalLatitude, decimalLongitude, elevationInMeters = Elevation, slopeInPercent = Slope, aspectInDegrees = Aspect)

  return(raw_data)
}

#' Wrangle UCBN site visits table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on 'UCBN Aspen Survey v1'
#'
#' @return raw_data with updated sites table
wrangleUCBNSiteVisist <- function(raw_data) {
  raw_data$data$SiteVisit <- raw_data$data$SiteVisit %>%
    dplyr::mutate(
      # Expand park names
      unitName = dplyr::case_when(
        Park == "CIRO" ~ "City of Rocks National Reserve",
        Park == "CRMO" ~ "Craters of the Moon National Monument and Preserve"),
      # Format date
      eventDate = as.Date(VisitDate),
      # Expand protocol version code to match value in Access data
      protocolVersion = ifelse(ProtocolVersion == 1, "ASPN_1_0", ProtocolVersion)) %>%
    dplyr::relocate(unitName, .after = Park) %>%
    dplyr::left_join(raw_data$data$Locations,
                     by = join_by(Unique_ID == Loc_Name)) %>%
    # Order cols
    dplyr::select(globalid, unitCode = Park, unitName, Stand, Transect, plotNumber = PlotNum,
                  plotName = Unique_ID, eventDate, standHeightInMeters = Site_Height,
                  verbatimCoordinateSystem:aspectInDegrees, protocolVersion, VisitNotes)

  return(raw_data)
}

#' Wrangle UCBN disturbances table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on 'UCBN Aspen Survey v1'
#'
#' @return raw_data with updated sites table
wrangleUCBNDisturbances <- function(raw_data) {
  raw_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, unitCode:eventDate) %>%
    dplyr::right_join(raw_data$data$Disturbances,
                      by = "parentglobalid") %>%
    # dplyr::mutate(Disturbance = ) %>% # expand out codes like ArborHist
    dplyr::select(-parentglobalid, -globalid)

  return(raw_data)
}

#' Wrangle UCBN observations table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on 'UCBN Aspen Survey v1'
#'
#' @return raw_data with updated sites table
wrangleUCBNObservations <- function(raw_data) {
  raw_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::select(parentglobalid = globalid, unitCode:eventDate) %>%
    dplyr::right_join(raw_data$data$Observations,
                      by = "parentglobalid") %>%
    # Isolate species name
    dplyr::mutate(verbatimIdentification = gsub("\\s*\\([^)]*\\)", "", sppSummaryCode)) %>%
    dplyr::select(-parentglobalid, -(sppSummaryCode:cntClass6List)) %>%
    # Pivot to tidy format
    tidyr::pivot_longer(cols = dplyr::contains("class"),
                        names_to = "sizeClass",
                        values_to = "individualCount") %>%
    dplyr::mutate(
      # Display class sizes as roman numerals to match UCBN data and protocol
      sizeClass = dplyr::case_when(
        sizeClass == "Class1" ~ "Class I",
        sizeClass == "Class2" ~ "Class II",
        sizeClass == "Class3" ~ "Class III",
        sizeClass == "Class4" ~ "Class IV",
        sizeClass == "Class5" ~ "Class V",
        sizeClass == "Class6" ~ "Class VI"
      ),
      # Add class descriptions
      sizeClassDescription = dplyr::case_when(
        sizeClass == "Class I" ~ "Suckers or seedlings less than 46 cm tall",
        sizeClass == "Class II" ~ "Suckers or seedlings 46 cm to 152 cm tall",
        sizeClass == "Class III" ~ "Greater than 152 cm and up to 2.5 cm in dbh",
        sizeClass == "Class IV" ~ "Greater than 2.5 cm in dbh and shorter than 75% of the stand height",
        sizeClass == "Class V" ~ "Greater than 2.5 cm in dbh and taller than 75% of the stand height",
        sizeClass == "Class VI" ~ "Dead stems greater than 2.5 cm in dbh"
      ))

  # Remove ID col from SiteVisit tbl, not needed after join
  raw_data$data$SiteVisit["globalid"] <- NULL

  return(raw_data)
}

#' Wrangle UCBN pests table
#'
#' @param raw_data Data table returned by fetchagol::fetchRawData on 'UCBN Aspen Survey v1'
#'
#' @return raw_data with updated sites table
wrangleUCBNPests <- function(raw_data) {
  raw_data$data$Pests <- raw_data$data$Observations %>%
    dplyr::select(parentglobalid = globalid, unitCode:verbatimIdentification) %>%
    dplyr::distinct() %>%
    dplyr::right_join(raw_data$data$Pests,
                      by = join_by("parentglobalid")) %>%
    dplyr::mutate(
      # Temporary hard codes - get lookups from Jeff
      # Expand pest shorthands
      pest = dplyr::case_when(
        Pest == "Borer" | Pest == "WoodBorer" ~ "Bark/Wood Boring Insect",
        #Pest == "Cankers" ~ "Cankers",
        Pest == "Defol" ~ "Defoliating Insect",
        Pest == "FoliageD" ~ "Foliage Disease",
        #Pest == "Gall" ~ "Gall",
        Pest == "Mistletoe" ~ "Dwarf Mistletoe",
        Pest == "MPineBeetle" ~ "Mountain Pine Beetle",
        Pest == "StemDecay" ~ "Stem Decay",
        Pest == "WPBRust" ~ "White Pine Blister Rust",
        TRUE ~ Pest
      ),
      # Add pest evidence descriptions
      pestDescription = dplyr::case_when(
        pest == "Bark/Wood Boring Insect" ~ "Entrance/exit holes in bark, frass.",
        pest == "Cankers" ~ "Lesions on bark, often with broken, callused, sooty, discolored, or weeping/bleeding bark. Fruiting bodies may be present or absent.",
        pest == "Defoliating Insect" ~ "Leaves partially or entirely eaten by insects, also larva droppings, leaves folded into shelters, silk.",
        pest == "Foliage Disease" ~ "Dark or discolored spots or patches on leaves, curled leaves, or leaves completely brown.",
        pest == "Gall" ~ "Irregularly swollen areas on twigs and branches.",
        pest == "Dwarf Mistletoe" ~ "Parasitic plant growing on conifer branches.",
        pest == "Mountain Pine Beetle" ~ "Pitch tubes, frass, J-shaped galleries under bark on pines.",
        pest == "Stem Decay" ~ "Fungal conks on stem.",
        pest == "White Pine Blister Rust" ~ "Cankers, swelling, broken bark, aecia, pitching, and chewing, on white pine species.",
      )
    ) %>%
    dplyr::select(-parentglobalid, -globalid, -Pest)

  # Remove ID col from Observations tbl, not needed after join
  raw_data$data$Observations["globalid"] <- NULL

  return(raw_data)
}
