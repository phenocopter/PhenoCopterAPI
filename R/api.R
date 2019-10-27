#' A R package to communicate with PhenoCopter platform.
#'
#' @export
phenocopter <- function() {

}

#' Login the PhenoCopter platform
#'
#' @param host The host of PhenoCopter API
#' @param email The user email
#' @param password The user password
pc_login <- function(host = PC_OPTIONS('host'),
                     email = PC_OPTIONS('email'),
                     password = PC_OPTIONS('password')) {
    # Update options
    pc_options(host = host, email = email, password = password)
    body <- list(Email = email,
                 Password = password,
                 RememberMe = "true")
    # login
    r <- request(httr::POST, path = 'login', body = body, encode = "form")

    if (r$status_code != 200) {
        stop('Wrong user name or password.')
    }

}
#' Logout PhenoCopter API
pc_logout <- function() {
    r <- request(httr::POST, path = 'logout')
    if (r$status_code != 200) {
        stop('Not log out.')
    }
}



#' Perform a request to Phenocopter
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param ... Other arguments of request
#'
#' @return The contents of response
#' @export
request <- function(method,
                    path = '/',
                    query = list(),
                    ...) {
    # Remove the leading "/" if it has one.
    path <- gsub('^/*(.*)$', "\\1", path)
    # Get the base commands in the senaps
    commands <- strsplit(path, '/')[[1]][1]

    host <- PC_OPTIONS('host')

    url <- httr::modify_url(host,
                            path = gsub("/+", "/",
                                        paste(httr::parse_url(host)$path, path, sep = "/")))

    response <- method(url, query = query, ...)
    response
}


.stop_for_status <- function(x) {
    if (httr::status_code(x) < 300) {
        return(invisible(x))
    }
    sts <- httr::http_status(x)
    msg <- paste0('Error at request "', x$url, '" with reason "',
                  sts$reason, '" and message "', sts$message, '"')
    stop(msg)
}
