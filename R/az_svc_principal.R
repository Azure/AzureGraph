#' Service principal in Azure Active Directory
#'
#' Base class representing an AAD service principal.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this service principal.
#' - `properties`: The service principal properties.
#' @section Methods:
#' - `new(...)`: Initialize a new service principal object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a service principal. By default, ask for confirmation first.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_service_principal` and `get_service_principal` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual service principal.
#'
#' @seealso
#' [ms_graph], [az_app]
#'
#' [Azure Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @format An R6 object of class `az_service_principal`.
#' @export
az_service_principal <- R6::R6Class("az_service_principal",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the service principal '", self$properties$displayName,
                          "'? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("servicePrincipals", self$properties$id)
        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    },

    print=function(...)
    {
        cat("<Graph service principal '", self$properties$displayName, "'>\n", sep="")
        cat("  app id:", self$properties$appId, "\n")
        cat("  directory id:", self$properties$id, "\n")
        cat("  app tenant:", self$properties$appOwnerOrganizationId, "\n")
        invisible(self)
    }
),

private=list(

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    }
))
