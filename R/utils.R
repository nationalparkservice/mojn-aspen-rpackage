#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

#' Get AGOL authentication token
#'
#' Treat this token as you would a password: don't hard-code it in your scripts or save it to a file. It will expire after 60 minutes.
#'
#' @param agol_username AGOL headless account username
#' @param agol_password AGOL headless account password (do not hard code this into your scripts!)
#' @param root NPS users should keep the default. See <https://developers.arcgis.com/rest/users-groups-and-items/root.htm> for more information.
#' @param referer NPS users should keep the default. See <https://developers.arcgis.com/rest/users-groups-and-items/generate-token.htm> for more information.
#'
#' @return An AGOL authentication token
#' @export
#'
fetchAGOLToken <- function(agol_username, agol_password = keyring::key_get(service = "AGOL", username = agol_username), root = "nps.maps.arcgis.com", referer = "https://irma.nps.gov") {

  url <- paste0("https://", root, "/sharing/rest/generateToken")

  # Get a token with a headless account
  token_resp <- httr::POST(url,
                           body = list(username = agol_username,
                                       password = agol_password,
                                       expiration = 60,
                                       referer = referer,
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))

  return(agol_token)
}

#' Fetch tabular data from AGOL
#'
#' Retrieves tabular data from AGOL layers and tables, even when number of rows exceeds maximum record count.
#'
#' @param data_path Feature service URL
#' @param layer_number Layer number
#' @param token Authentication token (not needed for public layers)
#' @param geometry Include spatial data columns? Works with points, not tested with other geometry types
#' @param where Query clause specifying a subset of rows (optional; defaults to all rows). See AGOL REST API documentation.
#' @param outFields String indicating which fields to return (optional; defaults to all fields). See AGOL REST API documentation.
#'
#' @return A tibble
#' @export
#'
fetchAllRecords <- function(data_path, layer_number, token, geometry = FALSE, where = "1=1", outFields = "*") {
  result <- tibble::tibble()
  exc_transfer <- TRUE
  offset <- nrow(result)

  qry <- list(where = where,
              outFields = outFields,
              f = "JSON",
              resultOffset = offset)

  if (!missing(token)) {
    qry$token <- token$token
  }

  while(exc_transfer) {
    resp <- httr::GET(paste0(data_path, "/", layer_number, "/query"),
                      query = qry)

    content <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))

    if ("error" %in% names(content)) {
      message <- glue::glue("Error code {content$error$code}: {content$error$message}")
      if ((content$error$message != content$error$details) && (content$error$details != '')) {
        message <- c(message, glue::glue("Details: {content$error$details}"))
      }
      names(message) <- rep("x", length(message))
      cli::cli_abort(message)
    }

    if ("exceededTransferLimit" %in% names(content)) {
      exc_transfer <- content$exceededTransferLimit
    } else {
      exc_transfer <- FALSE
    }

    if (geometry) {
      partial_result <- cbind(content$features$attributes, content$features$geometry) %>%
        dplyr::mutate(wkid = content$spatialReference$wkid) %>%
        tibble::as_tibble()
    } else {
      partial_result <- tibble::as_tibble(content$features$attributes)
    }
    result <- rbind(result, partial_result)
    offset <- nrow(result)
    qry$resultOffset <- offset
  }
  return(result)
}

#' Fetch metadata from AGOL
#'
#' Retrieves metadata from AGOL layers and tables.
#'
#' @param url Feature service URL
#' @param layer_number Optional layer ID
#' @param token Authentication token (not needed for public layers)
#'
#' @return A list
#' @export
#'
fetchMetadata <- function(url, token, layer_number) {

  if (!missing(layer_number)) {
    url <- paste0(url, "/", layer_number, "/metadata")
  } else {
    url <- paste0(url, "/info/metadata")
  }

  # Get metadata
  if (!missing(token)) {
    resp <- httr::GET(url,
                      query = list(token = token$token,
                                   format = "fgdc",
                                   f = "xml"))
  } else {
    resp <- httr::GET(url)
  }
  content <- httr::content(resp, type = "text/xml", encoding = "UTF-8")
  metadata <- xml2::as_list(content)
  metadata <- wrangleLayerMetadata(metadata$metadata, token)

  return(metadata)
}

wrangleMetadata <- function(raw_meta) {
  meta <- lapply(raw_meta$eainfo, function(entity) {
    table_name <- entity$detailed$enttyp$enttypl[[1]]
    item_meta <- list(table_name = list(table_description = entity$detailed$enttyp$enttypd[[1]]))
  })
}

wrangleLayerMetadata <- function(raw_meta, token) {
  # Field level metadata
  fields <- lapply(raw_meta$eainfo$detailed[2:length(raw_meta$eainfo$detailed)], function(field) {
    field_name <- field$attrlabl[[1]]
    desc <- parseAttrDef(field$attrdef[[1]])
    try({
      lookup_name <- trimws(field$attrdomv$codesetd$codesetn[[1]])
      lookup_url <- trimws(field$attrdomv$codesetd$codesets[[1]])
      lookup_df <- fetchHostedCSV(stringr::str_remove(lookup_url, "^.*?id="), token)
      desc$lookup <- list(lookup_name = lookup_name,
                          lookup_url = lookup_url,
                          lookup_df = lookup_df)
    }, silent = TRUE)
    item_meta <- list()
    item_meta[[field_name]] <- desc
    return(item_meta)
  })

  # simplify list
  fields <- purrr::flatten(fields)

  # Table level metadata
  table_name <- trimws(raw_meta$eainfo$detailed[1]$enttyp$enttypl[[1]])
  table_desc <- trimws(raw_meta$eainfo$detailed[1]$enttyp$enttypd[[1]])

  meta <- list(table_name = table_name,
               table_description = table_desc,
               fields = fields)

  return(meta)
}

parseAttrDef <- function(def) {
  attrs <- list()
  if (!is.null(def)) {
    description <- trimws(stringr::str_remove_all(def, "\\{.*\\}"))
  } else {
    description <- NA
  }

  if (any(grepl("\\{.*\\}", def))) {
    starts <- stringr::str_locate_all(def, "\\{")[[1]][, 1]
    ends <- stringr::str_locate_all(def, "\\}")[[1]][, 1]

    for (i in 1:length(starts)) {
      start <- starts[i] + 1
      end <- ends[i] - 1
      name_value <- trimws(strsplit(substr(def, start, end), ":")[[1]])
      attrs[[name_value[1]]] <- name_value[2]
    }
  }
  def_list <- list(description = description,
                   attributes = attrs)

  return(def_list)
}

#' Fetch feature service info from AGOL
#'
#' Retrieves metadata from AGOL layers and tables.
#'
#' @param url Feature service URL
#' @param token Authentication token (not needed for public layers)
#'
#' @return A list
#' @export
#'
fetchLayerAndTableList <- function(url, token) {

  qry <- list(f = "json")

  # Get feature service info
  if (!missing(token)) {
    qry$token <- token$token
  }

  resp <- httr::GET(url,
                    query = qry)

  content <- httr::content(resp, type = "text/json", encoding = "UTF-8")
  feature_service <- jsonlite::fromJSON(content)

  # TODO: there are no aspen feature layers in the aspen database, but there is one separate feature layer that we will need to import later

  # Get layer id's and names
  if (hasName(feature_service, "layers") & length(feature_service$layers) > 0) {
    layers <- dplyr::select(feature_service$layers, id, name)
  } else {
    layers <- tibble::tibble(.rows = 0)
  }

  # Get table id's and names
  if (hasName(feature_service, "tables") & length(feature_service$tables) > 0) {
    tables <- dplyr::select(feature_service$tables, id, name)
  } else {
    tables <- tibble::tibble(.rows = 0)
  }

  layers_tables <- rbind(layers, tables)
  # layers_tables <- layers

  return(layers_tables)
}

fetchRawAspen <- function(aspen_database_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Aspen_Test_Visit_NonSpatial_gdb/FeatureServer", agol_username = "mojn_data", agol_password = keyring::key_get(service = "AGOL", username = agol_username)) {
  token <- fetchAGOLToken(agol_username, agol_password)
  layers_tables <- fetchLayerAndTableList(aspen_database_url, token)
  ids <- layers_tables$id
  names(ids) <- layers_tables$name

  metadata <- sapply(ids, function(id) {
    meta <- fetchMetadata(aspen_database_url, token, id)
    meta[["table_id"]] <- id
    return(meta)
  }, simplify = FALSE, USE.NAMES = TRUE)

  data <- sapply(metadata, function(meta){
    data_table <- fetchAllRecords(aspen_database_url, meta$table_id, token, outFields = paste(names(meta$fields), collapse = ",")) %>%
      dplyr::select(dplyr::any_of(names(meta$fields)))
    return(data_table)
  }, simplify = FALSE, USE.NAMES = TRUE)

  raw_data <- list(data = data,
                   metadata = metadata)
  return(raw_data)
}
