#' Extensible registry of Microsoft Graph classes that AzureGraph supports
#'
#' @param name The name of the Graph class, eg "user", "servicePrincipal", etc.
#' @param R6_generator An R6 class generator corresponding to this Graph class.
#' @param check_function A boolean function that checks if a list of properties is for an object of this class.
#' @details
#' As written, AzureGraph knows about a subset of all the object classes contained in Microsoft Graph. These are mostly the classes originating from Azure Active Directory and Microsoft Office: users, groups, app registrations, service principals, SharePoint sites and OneDrive libraries.
#'
#' You can extend AzureGraph by writing your own R6 class that inherits from `ms_object`. If so, you should also _register_ your class by calling `register_graph_class` and providing the generator object, along with a check function. The latter should accept a list of object properties (as obtained from the Graph REST API), and return TRUE/FALSE based on whether the object is of your class.
#'
#' @return
#' An invisible vector of registered class names.
#' @examples
#' \dontrun{
#'
#' # built-in 'az_user' class, for an AAD user object
#' register_graph_class("user", az_user,
#'    function(props) !is.null(props$userPrincipalName))
#'
#' }
#' @export
register_graph_class <- function(name, R6_generator, check_function)
{
    if(!R6::is.R6Class(R6_generator))
        stop("R6_generator should be an R6 class generator object", call.=FALSE)
    if(!is.function(check_function))
        stop("check_function should be a function")

    .graph_classes[[name]] <- list(
        generator=R6_generator,
        check=check_function
    )
    invisible(ls(.graph_classes))
}


.graph_classes <- new.env()

# classes supplied by AzureGraph
register_graph_class("user", az_user,
    function(props) !is.null(props$userPrincipalName))

register_graph_class("group", az_group,
    function(props) !is.null(props$groupTypes))

register_graph_class("application", az_app,
    function(props) !is.null(props$appId) && is.null(props$servicePrincipalType))

register_graph_class("servicePrincipal", az_service_principal,
    function(props) !is.null(props$appId) && !is.null(props$servicePrincipalType))

register_graph_class("device", az_device,
    function(props) !is.null(props$publishingState))

register_graph_class("directoryRole", az_directory_role,
    function(props) !is.null(props$roleTemplateId))

find_class_generator <- function(props, type_filter, default_generator)
{
    # use ODATA metadata if available
    if(!is.null(props$`@odata.type`))
    {
        type <- sub("^#microsoft.graph.", "", props$`@odata.type`)
        if(!(type %in% ls(.graph_classes)))
            type <- NA
    }
    else  # check for each known type in turn
    {
        found <- FALSE
        type <- NA
        for(n in ls(.graph_classes))
        {
            found <- .graph_classes[[n]]$check(props)
            if(found)
            {
                type <- n
                break
            }
        }
    }

    # here, 'type' will be one of the known types or NA for unknown
    # always return the default class if unknown, even if a type filter is provided
    if(is.na(type))
        return(default_generator)

    if(is.null(type_filter) || type %in% type_filter)
        .graph_classes[[type]]$generator
    else NULL
}

