
.check_id <- function(id) {
    if (!(class(id) %in% c('numeric', 'integer') && length(id) == 1)) {
        stop('Provide the correct flight id.')
    }
}
#' Get a flight information
#'
#' @param id The flight id
#'
#' @return A list with flight information
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight(1)
#' }
get_flight <- function(id) {
    .check_id(id)
    response <- request(httr::GET, paste0('flight/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}



#' Get GCPs for a flight
#'
#' @param id The flight id
#'
#' @return A list with GCP information
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_gcp(1)
#' }
get_flight_gcp <- function(id) {
    .check_id(id)
    response <- request(httr::GET, paste0('flight/', id, '/gcp'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


#' Get images for a flight
#'
#' @param id The flight id
#'
#' @return A list with images information
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_image(1)
#' }
get_flight_image <- function(id) {
    .check_id(id)
    response <- request(httr::GET, paste0('flight/', id, '/images'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


