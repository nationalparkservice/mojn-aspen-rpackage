# Assign global variables to avoid the "no visible binding for global variable 'x'" error in build checks
globalVariables(c("latlong_wkid"))

#' Write loaded and wrangled aspen data to CSV files
#'
#' @param aspen_data List of aspen data frames in data package format, as returned by `load_and_package_aspen()`.
#'
#' @description
#' This function creates a folder in your working directory called aspen_data
#' and writes the loaded and wrangled tables to CSV with the naming format NTWK_Aspen_Tbl.csv.
#' If a folder called aspen_data already exists in your working directory,
#' it will overwrite existing files of the same name within that folder.
#'
#' This function
#' @returns A folder called aspen_data_package with a CSV file for each data table
#' @export
write_aspen <- function(aspen_data) {
  # Input validation
  if (!"data" %in% names(aspen_data) || !all(sapply(aspen_data$data, is.data.frame))) {
    stop("Data must be in the format returned by `load_and_package_aspen()`.")
  }

  network <- .get_network(aspen_data)

  file_path <- here::here("aspen_data")

  if(!dir.exists(file_path)) {
    dir.create(file_path)
  }

  for (tbl in names(aspen_data$data)) {
    # Standard format file names
    tbls <- paste0(network, "_Aspen_", tbl, ".csv")

    # Write each table to folder
    utils::write.csv(aspen_data$data[[tbl]],
                     file = file.path(file_path, tbls),
                     row.names = FALSE,
                     fileEncoding = "UTF-8")
  }
}

#' Get network based on content of data
#'
#' @param data List of aspen dataframes
#' @param raw Logical value where TRUE indicates the function is being used on raw data and FALSE indicates it is being used on packaged data.
#'
#' @returns Network code
#' @noRd
.get_network <- function(data, raw = FALSE) {
  if (raw) {
    # for use in wrangle and packages functions
    if ("AllSites" %in% names(data$data)) {
      network <- "MOJN"
    } else {
      network <- "UCBN"
    }
  } else {
    # for use in QC, vis functions
    if ("GRBA" %in% unique(data$data$SiteVisit$unitCode)) {
      network <- "MOJN"
    } else {
      network <- "UCBN"
    }
  }
  return(network)
}

#' Fetch elevation, aspect, and slope from AGOL for UCBN site coordinates
#'
#' @description
#' Wrapper function for `ucbn::FetchElevationAspectSlope`. Fetches elevation,
#' aspect, and slope values for UCBN site coordinates the first time it is run and
#' saves them as ucbn_elevation_aspect_slope.rds on the user's machine. On subsequent
#' runs, reads in rds file and only fetches values for new coordinates. Run
#' `file.path(tools::R_user_dir("aspen", "data"))` to get the file location and
#' delete the rds file to generate all new values.
#'
#' @param ucbn_locations raw_data$data$Locations table in `wrangle_ucbn_sites` after some processing
#' @param agol_username AGOL headless account username as supplied to `load_and_package_aspen()`
#'
#' @returns raw_data$data$Locations with elevation, aspect and slope values
#' @noRd
fetch_ucbn_eas <- function(ucbn_locations, agol_username) {
  # Get file path and read in rds if exists
  cache_file <- file.path(tools::R_user_dir("aspen", "data"), "ucbn_elevation_aspect_slope.rds")
  cache <- if (file.exists(cache_file)) readRDS(cache_file) else NULL

  # Get coordinates from ucbn locations table
  coord <- ucbn_locations %>%
    dplyr::select(decimalLatitude, decimalLongitude, latlong_wkid) %>%
    dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
    dplyr::distinct()

  # Check coordinates against what is cached
  if (!is.null(cache)) {
    coord <- dplyr::anti_join(coord, cache, by = c("decimalLatitude", "decimalLongitude"))
  }
  # Get eas for new coord if there are any
  if (nrow(coord) > 0) {
    new_eas <- ucbn::FetchElevationAspectSlope(coord,
                                               lat_col = "decimalLatitude",
                                               long_col =  "decimalLongitude",
                                               wkid_col = "latlong_wkid",
                                               agol_username = agol_username) %>%
      dplyr::select(decimalLatitude, decimalLongitude, Elevation, Slope, Aspect)
    # Combine with existing
    cache <- dplyr::bind_rows(cache, new_eas)
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(cache, cache_file)
  }

  ucbn_locations %>%
    dplyr::select(-dplyr::any_of(c("Elevation", "Slope", "Aspect"))) %>%
    dplyr::left_join(cache, by = c("decimalLatitude", "decimalLongitude"))
}

#' reactable formatting for quality control tables
#'
#' @description
#' Applies consistent formatting to quality control tables to be displayed in Rmd
#' documents. For all tables: applies consistent color scheme, sizes column widths,
#' includes a search bar, turns of wrapping for cell contents.
#'
#' Additional optional formatting includes grouped cell shading when column names
#' are supplied using shade_by param and proportional cell coloring when both
#' `missing_row_color = TRUE` and total_rows param are supplied. 0% of total row
#' count has no color, 100% of total row count is red. Values between range from
#' light yellow to orange.
#'
#' @param table Table as produced by one of the aspen quality control functions
#' @param shade_by List of column names for row shading by group. Used in `check_size_class`
#' @param missing_row_color Logical. Used in conjunction with total_rows param; TRUE for proportional cell shading in `count_missing_values()`
#' @param total_rows Numeric. Total number of rows in original table. Used in conjunction with missing_row_color param for proportional cell shading in `count_missing_values()`
#'
#' @return The table with reactable formatting
#' @noRd
format_qc <- function(table, shade_by = NULL, missing_row_color = FALSE, total_rows = NULL) {

  # Color shading for check_missing_values()
  num_colors <- NULL
  if (missing_row_color && !is.null(total_rows)) {
    # assign colors
    num_colors <- function(value) {
      if (value == 0) { return(NULL) } # no color for 0 count
      color <- if (value <= total_rows * 0.25) {
        "#FFEC8B"
      } else if (value <= total_rows * 0.50) {
        "#FFD700"  #EEC900
      } else if (value <= total_rows * 0.75) {
        "#FFA500"  #FFB90F
      } else  if (value < total_rows) {
        "#FF7F00"
      } else {
        "#FF4500" # red for entirely NA colS
      }
      list(background = color)
    }
  }

  # Column specs
  columns <- stats::setNames(
    lapply(names(table), function(col) {
      # Get longest number of characters from either col name or col value
      longest <- max(nchar(col), nchar(as.character(table[[col]])), na.rm = TRUE)
      reactable::colDef(
        # Calculate minimum pixel width, either 100 or based on longest char
        minWidth = max(100, max(80, longest * 10 + 40)),
        # Apply colors if missing_row_color and total_rows are supplied
        style = if (missing_row_color && !is.null(total_rows)) num_colors else NULL)
    }),
    names(table)
  )

  # Row shading by group for check_site_visits(), check_size_class(),
  # check_duplicate_pests, check_duplicate_disturbances()
  row_style <- NULL
  if (!is.null(shade_by)) {
    shade <- do.call(dplyr::consecutive_id, unname(as.list(table[shade_by]))) %% 2 == 0
    row_style <- function(index) {
      if (shade[index]) list(background = "#EDEDED")
    }
  }

  # All tables use color scheme, have search bar, don't wrap text
  reactable::reactable(
    table,
    searchable = TRUE,
    wrap = FALSE,
    defaultPageSize = 15,
    theme = reactable::reactableTheme(
      backgroundColor = "#FCFCFC",
      borderColor = "#556B2F"
    ),
    columns = columns,
    defaultColDef = reactable::colDef(
      align = "left",
      headerStyle = list(background = "#EEEEE0",
                         whiteSpace = "nowrap")),
    rowStyle = row_style
  )
}
