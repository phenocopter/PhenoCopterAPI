# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   07:36 AM Thursday, 25 July 2019
# * Copyright: AS IS


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
get_flight_image <- function(flight) {
    .check_id(flight)
    response <- request(httr::GET, paste0('flight/', flight, '/images'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


add_flight_path <- function(flight, data, overwrite = FALSE) {
    .check_id(flight)
    images <- get_flight_image(flight)
    if (length(images) > 0 & overwrite == FALSE) {
        stop('Image information already stored into database. Use overwrite = TRUE to overwrite them')
    }
    if (overwrite == TRUE) {
        stop("Not implemented overwrite = TRUE.")
    }
    # Check columns
    cols <- c('flightId', 'latitude', 'longitude', 'elevation', 'width', 'height',
              'datetimeOriginal', 'original')
    pos <- cols %in% names(data)
    if (sum(pos) != length(cols)) {
        stop('Cannot find columns: ', paste(cols[!pos], collapse = ', '))
    }
    # Check the time
    if (!('POSIXct' %in% class(data$datetimeOriginal))) {
        stop('datetimeOriginal column should be a timestamp format')
    }
    # Check flight ID
    fid <- unique(data$flightId)
    if (length(fid) > 1) {
        stop('Multiple flight ID is in the data frame')
    }
    if (fid != flight) {
        stop('Flight ID is not matched')
    }

    # Generate files for each capture point in case of multiple band flight
    generate_file <- function(df) {
        res <- df %>%
            select(flightId, latitude, longitude, elevation, width, height, datetimeOriginal) %>%
            slice(1)
        # Generate band if it is missing
        if (!has_name(df, 'band')) {
            df$band <- seq_len(nrow(df))
        }
        res$imageFile = list(df %>%
                                 select(original, band))
        res
    }
    new <- data %>%
        mutate(datetimeOriginal = format(datetimeOriginal, format = '%Y-%m-%dT%H:%M:%S%z')) %>%
        group_by(flightId, latitude, longitude, elevation, width, height, datetimeOriginal) %>%
        do(generate_file(.))

    response <- request(httr::POST, paste0('flight/', flight, '/images'),
                        body = jsonlite::toJSON(new, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}
