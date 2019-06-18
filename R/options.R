# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:40 PM Saturday, 09 June 2018
# * Copyright: AS IS


# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
PC_OPTIONS <- settings::options_manager(host = Sys.getenv("PC_HOST"),
                                                 email = Sys.getenv("PC_EMAIL"),
                                                 password = Sys.getenv("PC_PASSWORD"))


#' Set or get options for PhenoCopterAPI package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{host}}{ The host  for PhenoCopter API }
#'  \item{\code{email}}{ The email for PhenoCopter API}
#'  \item{\code{password}}{ The password for PhenoCopter API}
#' }
#'
#' @export
pc_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    PC_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @export
pc_reset <- function() {
    settings::reset(PC_OPTIONS)
}
