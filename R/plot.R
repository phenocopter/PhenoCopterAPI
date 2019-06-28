# API related with plots


get_plot_segmentation <- function(id) {
    response <- request(httr::GET, paste0('flight/', id, '/plot/segmentations'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}