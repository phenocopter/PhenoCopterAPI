

#' Get workflow for all flights
#'
#' @description
#' All workflows with status if workflow id or name is null, status id or name is null.
#' Otherwise, results filter by workflow and status. Only admin group allows to call this api.
#' @param workflow_id Workflow id
#' @param workflow_name Workflow name
#' @param status_id status id
#' @param status_name status name
#' @return A list with workflow information
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_workflows(workflow_id = 1, staus_id = 1)
#' get_flight_workflows(workflow_name = "stitching_mosaic", staus_name = "Schedule")
#' }
get_flight_workflows <- function(workflowId = NULL, workflowName = "",
                                 statusId = NULL, statusName = "") {

    response <- request(httr::GET, 'flight/workflows',
                        query = list(workflowId = workflowId,
                                     workflowName = workflowName,
                                     statusId = statusId,
                                     statusName = statusName))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}




#' Get workflow for a flight
#'
#' @param flight Flight id
#' @return A list with workflow information
#' @export
#'
#' @examples
#' \dontrun{
#' pc_login()
#' get_flight_workflow(1)
#' }
get_flight_workflow <- function(flight, workflow = NULL, fun = NULL) {

    url <- paste0('flight/', flight, '/workflow')
    if (!is.null(workflow)) {
        url <- paste0(url, '/', workflow)
        if (!is.null(fun)) {
            url <- paste0(url, '?fun=', fun)
        }
    }

    response <- request(httr::GET, url)
    .stop_for_status(response)
    response <- httr::content(response)
    response
}




#' PUT workflow for a flight
#'
#' @description
#' PUT the workflow into system
#' @param flight Flight id
#' @param id id for flight workflow
#' @param workflow A new workflow
#' @return The new workflow
#' @export
#' @examples
#' \dontrun{
#' pc_login()
#' put_flight_workflow(flight = 1, id = 1, workflow = list(id = 1, workflowId = 1, statusId = 1))
#' }
put_flight_workflow <- function(flight, id, workflow) {

    workflow$flightId <- flight
    workflow$id <- id
    response <- request(httr::PUT, paste0('flight/', flight, '/workflow/', id),
                        body = jsonlite::toJSON(workflow, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    .stop_for_status(response)
    response <- httr::content(response)
    response
}



#' PUT workflow for a flight
#'
#' @description
#' PUT the workflow into system
#' @param flight Flight id
#' @param id id for flight workflow
#' @param increment New progress
#' @return The new workflow
#' @export
#' @examples
#' \dontrun{
#' pc_login()
#' put_flight_workflow(flight = 1, id = 1, workflow = list(id = 1, workflowId = 1, statusId = 1))
#' }
put_flight_workflow_increment <- function(flight, id, increment) {

    response <- request(httr::PUT, paste0('flight/', flight, '/workflow/', id, "/progress/increment/", increment),
                        config = httr::content_type('application/json'))


    .stop_for_status(response)
    response <- httr::content(response)
    response
}





#' Add new flight log
#'
#' @param flight Flight id
#' @param workflow workflow id
#' @param data a list of message
#'
#' @return
#' @export
post_flight_log <- function(flight, workflow, data) {
    .check_id(flight)

    response <- request(httr::POST,
                        paste0('flight/', flight, '/workflow/', workflow, '/log'),
                        body = jsonlite::toJSON(data, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    .stop_for_status(response)

    r <- httr::content(response)
    if (is.null(response$status_code) || !(response$status_code %in% c(200, 201))) {
        stop(r)
    }
    r
}


#' Delete flight log
#'
#' @param flight Flight id
#' @param workflow workflow id
#'
#' @return
#' @export
delete_flight_log <- function(flight, workflow, fun = NULL) {
    .check_id(flight)
    url <- paste0('flight/', flight, '/workflow/', workflow, '/log')
    if (!is.null(fun)) {
        url <- paste0(url, '?fun=', fun)
    }
    response <- request(httr::DELETE, url)
    .stop_for_status(response)

    r <- httr::content(response)
    r
}
