
#' Retrieve WMS layers for a flight
#'
#' @param flight flight id
#'
#' @return
#' @export
#'
#' @examples
get_flight_wms <- function(flight) {
    .check_id(flight)
    response <- request(httr::GET, paste0('flight/', flight, '/wms'))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}

#' Add a WMS layer for a flight
#'
#' @param flight Flight ID
#' @param name Name of vegetation index (case in-sensitive)
#' @param value Layer name of GeoServer
#' @param min Min value of layer (null in default)
#' @param max Max value of layer (null in default)
#' @param hide Whether this layer is hide in default
#'
#' @return
#' @export
add_flight_wms <- function(flight, name, value, min = NULL, max = NULL, hide = TRUE) {
    .check_id(flight)

    wms_exist <- get_flight_wms(flight)
    layers <- get_flight_layer(flight)
    vi_id <- NULL
    for (i in seq(along = layers)) {
        if (tolower(layers[[i]]$name) == tolower(name)) {
            vi_id <- layers[[i]]$id
            break;
        }
    }
    if (is.null(vi_id)) {
        stop(paste0('Vegetation Index ', name, ' is not found'))
    }
    url <- paste0('flight/', flight, '/wms')
    wms <- list(flightId = flight, vegetationIndexId = vi_id,
                value = value,
                hide = hide,
                min = min,
                max = max)
    # Check whether name is existing
    method <- httr::POST
    for (i in seq(along = wms_exist)) {
        if (tolower(wms_exist[[i]]$vegetationIndex$name) == tolower(name)) {
            method <- httr::PUT
            url <- paste0(url, '/', wms_exist[[i]]$id)
            wms$id <- wms_exist[[i]]$id
            break
        }
    }

    response <- request(method, url,
                        body = jsonlite::toJSON(wms, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}
