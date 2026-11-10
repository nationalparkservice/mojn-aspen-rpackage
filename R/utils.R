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
    # for use in QC functions
    if ("GRBA" %in% unique(data$data$SiteVisit$unitCode)) {
      network <- "MOJN"
    } else {
      network <- "UCBN"
    }
  }
  return(network)
}

#' Write loaded and wrangled aspen data to CSV files
#'
#' @param aspen_data List of aspen data frames in data package format, as returned by `loadAndPackageAspen()`.
#'
#' @description
#' This function creates a folder in your working directory called aspen_data_package
#' and writes the loaded and wrangled tables to CSV with the naming format NTWK_Aspen_Tbl.csv.
#' If a folder called aspen_data_package already exists in your working directory,
#' it will overwrite existing files of the same name within that folder.
#'
#' This function
#' @returns A folder called aspen_data_package with a CSV file for each data table
#' @export
writeAspen <- function(aspen_data) {
  # Input validation
  if (!"data" %in% names(aspen_data) || !all(sapply(aspen_data$data, is.data.frame))) {
    stop("Data must be in the format returned by `loadAndWrangleAspen()`.")
  }

  network <- .get_network(aspen_data)

  file_path <- here::here("aspen_data_package")

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
