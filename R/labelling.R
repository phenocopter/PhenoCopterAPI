

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
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}