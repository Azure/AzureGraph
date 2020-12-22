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
    R6::is.R6(object) && inherits(object, "az_app")
}


#' @rdname info
#' @export
is_service_principal <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_service_principal")
}


#' @rdname info
#' @export
is_user <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_user")
}


#' @rdname info
#' @export
is_group <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_group")
}


#' @rdname info
#' @export
is_directory_role <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_directory_role")
}


#' @rdname info
#' @export
is_sharepoint_site <- function(object)
{
    R6::is.R6(object) && inherits(object, "ms_site")
}


#' @rdname info
#' @export
is_drive <- function(object)
{
    R6::is.R6(object) && inherits(object, "ms_drive")
}


#' @rdname info
#' @export
is_sharepoint_list <- function(object)
{
    R6::is.R6(object) && inherits(object, "ms_sharepoint_list")
}


#' @rdname info
#' @export
is_aad_object <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_object")
}


#' @rdname info
#' @export
is_msgraph_object <- function(object)
{
    R6::is.R6(object) && inherits(object, "ms_object")
}

