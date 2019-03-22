#' Informational functions
#'
#' These functions return whether the object is of the corresponding class.
#'
#' @param object An R object.
#'
#' @return
#' A boolean.
#' @rdname info
#' @export
is_app <- function(object)
{
    R6::is.R6(object) && inherits(object, "gr_app")
}


#' @rdname info
#' @export
is_service_principal <- function(object)
{
    R6::is.R6(object) && inherits(object, "gr_service_principal")
}


#' @rdname info
#' @export
is_user <- function(object)
{
    R6::is.R6(object) && inherits(object, "gr_user")
}


#' @rdname info
#' @export
is_group <- function(object)
{
    R6::is.R6(object) && inherits(object, "gr_group")
}
