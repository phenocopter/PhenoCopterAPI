# API related with plots


get_plot <- function(flight) {
    response <- request(httr::GET, paste0('flight/', flight, '/plots'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    map_df(response, as_tibble) %>%
        arrange(column, row)
}



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





post_plot_segmentation_image <- function(flight, data) {
    .check_id(flight)

    response <- request(httr::POST, paste0('flight/', flight, '/plot/segmentation/images'),
                        body = jsonlite::toJSON(data, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    r <- httr::content(response)
    if (is.null(response$status_code) || !(response$status_code %in% c(200, 201))) {
        stop(r)
    }
    r
}

delete_plot_segmentation_image <- function(flight) {
    .check_id(flight)

    response <- request(httr::DELETE, paste0('flight/', flight, '/plot/segmentation/images'),
                        body = jsonlite::toJSON(data, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    r <- httr::content(response)
    if (is.null(response$status_code) || !(response$status_code %in% c(200, 201))) {
        stop(r)
    }
    r
}




#' Get plot phenotype
#'
#' @param flight Flight id
#'
#' @return
#' @export
#'
#' @examples
get_plot_phenotypes <- function(flight, function_id = NULL) {
    query = list()
    if (!is.null(function_id)) {
        query[['function']] <- function_id
    }
    response <- request(httr::GET, paste0('flight/', flight, '/plot/phenotypes'),
                        query = query)
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
add_plot_phenotypes <- function(flight, fun, data) {
    .check_id(flight)

    url <- paste0('flight/', flight, '/fun/', fun, '/phenotypes')
    response <- request(httr::POST, url,
                        body = jsonlite::toJSON(data, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


#' Delete plot phenotype
#'
#' @param flight Flight id
#' @param function_id Flight function id
#'
#' @return
#' @export
#'
#' @examples
delete_plot_phenotypes <- function(flight, function_id = NULL) {
    query = list()
    if (!is.null(function_id)) {
        query[['function']] <- function_id
    }
    response <- request(httr::DELETE, paste0('flight/', flight, '/plot/phenotypes'),
                        query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}

