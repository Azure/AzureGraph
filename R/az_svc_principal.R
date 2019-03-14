#' Service principal in Azure Active Directory
#'
#' Base class representing an AAD service principal.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new service principal object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a service principal. By default, ask for confirmation first.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_service_principal` and `get_service_principal` methods of the [az_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the AD Graph API to create the actual service principal.
#'
#' @seealso
#' [az_graph], [az_app]
#'
#' [Azure AD Graph overview](https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-graph-api),
#' [REST API reference](https://docs.microsoft.com/en-au/previous-versions/azure/ad/graph/api/api-catalog)
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
            msg <- paste0("Do you really want to delete the '", self$properties$displayName,
                          "' service principal? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- if(!is_empty(self$properties$objectId))
            file.path("servicePrincipals", self$properties$objectId)
        else file.path("servicePrincipalsByAppId", self$properties$appId)

        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    }
),

private=list(

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, self$tenant, op, ...)
    }
))
