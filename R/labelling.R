

#' Get labelling for a flight
#'
#' @param id The flight id
#'
#' @return A list with labelling
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_labelling(1)
#' }
get_labelling <- function(id) {
    .check_id(id)
    response <- request(httr::GET, paste0('flight/', id, '/labelling'))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}

#' Get labelling of images for a flight
#'
#' @param id The flight id
#'
#' @return A list with labelling
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_labelling_image(1)
#' }
get_labelling_image <- function(flight) {
    .check_id(flight)
    response <- request(httr::GET, paste0('flight/', flight, '/labelling/image'))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}
