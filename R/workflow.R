

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
    httr::stop_for_status(response)
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
    
    response <- request(httr::PUT, paste0('flight/', flight, '/workflow/', id),
                        body = jsonlite::toJSON(workflow, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}

