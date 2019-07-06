# API related with plots


#' Get plot segmentaion
#'
#' @param flight Flight ID
#'
#' @return
#' @export
#'
#' @examples
get_plot_segmentation <- function(flight) {
    response <- request(httr::GET, paste0('flight/', flight, '/plot/segmentations'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}




#' Get plot phenotype
#'
#' @param flight Flight id
#'
#' @return
#' @export
#'
#' @examples
get_plot_phenotypes <- function(flight) {
    response <- request(httr::GET, paste0('flight/', flight, '/plot/phenotypes'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}



#' Add new plot phenotype
#'
#' @param flight Post
#'
#' @return
#' @export
#'
#' @examples
add_plot_phenotypes <- function(flight, data) {
    .check_id(flight)

    # wms_exist <- get_flight_wms(flight)
    # layers <- get_flight_layer(flight)
    # vi_id <- NULL
    # for (i in seq(along = layers)) {
    #     if (tolower(layers[[i]]$name) == tolower(name)) {
    #         vi_id <- layers[[i]]$id
    #         break;
    #     }
    # }
    # if (is.null(vi_id)) {
    #     stop(paste0('Vegetation Index ', name, ' is not found'))
    # }
    # url <- paste0('flight/', flight, '/wms')
    # wms <- list(flightId = flight, vegetationIndexId = vi_id,
    #             value = value,
    #             hide = hide,
    #             min = min,
    #             max = max)
    # # Check whether name is existing
    # method <- httr::POST
    # for (i in seq(along = wms_exist)) {
    #     if (tolower(wms_exist[[i]]$vegetationIndex$name) == tolower(name)) {
    #         method <- httr::PUT
    #         url <- paste0(url, '/', wms_exist[[i]]$id)
    #         wms$id <- wms_exist[[i]]$id
    #         break
    #     }
    # }
    #
    # response <- request(method, url,
    #                     body = jsonlite::toJSON(wms, auto_unbox = TRUE,
    #                                             null = 'null'),
    #                     config = httr::content_type('application/json'))
    # httr::stop_for_status(response)
    # response <- httr::content(response)
    # response
}
