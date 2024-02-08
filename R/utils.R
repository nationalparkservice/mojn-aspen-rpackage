#' @importFrom magrittr %>% %<>%
#' @import dplyr
#' @import fetchagol

pkg_globals <- new.env(parent = emptyenv())


#' Fetch aspen data from AGOL and do preliminary data wrangling
#'
#' @param aspen_url URL to main AGOL aspen database
#' @param site_url URL to AGOL database for aspen sites (if applicable)
#' @param agol_username Authentication token (not needed for public layers)
#'
#' @return A list of data frames and metadata
#' @export


# TODO make sure this works for everybody
# TODO make mojn master site data optional
# TODO should data be loaded from inside the function like now or input as a variable
# TODO agol_password should maybe be added as a variable???
# TODO can we remove creation, creationDate, editDate, and editor from all the tables??
fetchAndWrangleAspen <- function(aspen_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Aspen_Test_Visit_NonSpatial_gdb/FeatureServer",
                                 site_url =  "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/AspenSites2/FeatureServer",
                                 agol_username = "mojn_data") {

  # Import aspen database and aspen site database
  raw_data <- fetchagol::fetchRawData(aspen_url, agol_username)
  raw_site_data <- fetchagol::fetchRawData(site_url, agol_username)

  # Add site data to list of other data
  raw_data$data$AllSites <- raw_site_data$data$`MOJN Aspen Sites Master`
  # Join site metadata to list of other metadata
  raw_data$metadata$AllSites <- raw_site_data$metadata$`MOJN Aspen Sites Master`

  raw_data <- fetchagol::cleanData(raw_data)

  flattened_data <- list(data = list(),
                         metadata = list())


  # Join background site information with other tables
  flattened_data$data$AllSites <- raw_data$data$AllSites
  flattened_data$data$SiteVisit <- raw_data$data$SiteVisit

  flattened_data$data$Disturbances <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason) %>%
    dplyr::right_join(raw_data$data$Disturbances, by = c("parentglobalid" = "parentglobalid"))

  flattened_data$data$Observations <- raw_data$data$SiteVisit %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason) %>%
    dplyr::right_join(raw_data$data$Observations, by = c("parentglobalid" = "parentglobalid"))

  flattened_data$data$Pests <- flattened_data$data$Observations %>%
    dplyr::mutate(parentglobalid = globalid) %>%
    dplyr::select(parentglobalid, Park, Site, VisitType, VisitDate, FieldSeason, SpeciesCode) %>%
    dplyr::right_join(raw_data$data$Pests, by = c("parentglobalid" = "parentglobalid"))

  # TODO Figure out what to do with metadata, in iu its left empty
  flattened_data$metadata <- raw_data$metadata


  return(flattened_data)
}

formatMetadataAsEML <- function(meta, token) {
  attrs <- sapply(names(meta), function(attr_name) {
    meta_list <- meta[[attr_name]]
    if (meta_list$attributes$class == "date") {
      format_string <- "YYYY-MM-DD"
    } else if (meta_list$attributes$class == "dateTime") {
      format_string <- "YYYY-MM-DDThh:mm:ss"
    } else {
      format_string <- NA
    }
    attributes_df <- tibble::tibble(attributeName = attr_name,
                                    attributeDefinition = meta_list$description,
                                    unit = c(meta_list$attributes$unit, NA)[1],
                                    class = meta_list$attributes$class,
                                    dateTimeFormatString = format_string,
                                    missingValueCode = c(meta_list$attributes$missing_value_code, NA)[1],
                                    missingValueCodeExplanation = c(meta_list$attributes$missing_value_code_exp, NA)[1])

    return(attrs)
  }, simplify = TRUE)

  catvars <- sapply(names(meta), function(attr_name) {
    if (length(meta_list$lookup$lookup_url) > 0) {
      catvars_df <- meta_list$lookup$lookup_df %>%
        dplyr::mutate(attributeName = attr_name) %>%
        dplyr::select(attributeName, code = name, definition = description)
    } else {
      catvars_df <- NULL
    }
    return(catvars_df)
  }, simplify = TRUE)

}


# fetchHostedCSV <- function(item_id, token, root = "nps.maps.arcgis.com") {
#   url <- paste0("https://", root, "/sharing/rest/content/items/", item_id, "/data")
#   resp <- httr::GET(url, query = list(token = token$token))
#   content <- httr::content(resp, type = "text/csv", encoding = "UTF-8")
#
#   return(content)
# }
