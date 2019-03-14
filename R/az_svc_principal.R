#' Service principal in Azure Active Directory
#'
#' Base class representing an AAD service principal.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new service principal object. See 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a service principal. By default, ask for confirmation first.
#'
#' @section Initialization:
#' The recommended way to create a new service principal object is via the `create_service_principal` and `get_service_principal` methods of the [az_graph] and [az_app] classes, which will handle the details automatically.
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

    # need explicit mode arg because initialize(app_id) can either create a new SP or get an existing one
    initialize=function(token, tenant=NULL, app_id=NULL, object_id=NULL, ..., deployed_properties=list(), mode="get")
    {
        self$token <- token
        self$tenant <- tenant

        self$properties <- if(!is_empty(list(...)) || mode == "create")
            private$init_and_deploy(appId=app_id, ...)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(app_id, object_id)
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

        op <- file.path("servicePrincipals", self$properties$objectId)
        call_graph_endpoint(self$token, self$tenant, op, http_verb="DELETE")
        invisible(NULL)
    }
),

private=list(

    init_and_deploy=function(...)
    {
        properties <- list(...)

        call_graph_endpoint(self$token, self$tenant, "servicePrincipals", body=properties, encode="json", http_verb="POST")
    },

    init_from_parms=function(parms)
    {
        parms
    },

    init_from_host=function(app_id, object_id)
    {
        op <- if(is.null(object_id))
            file.path("servicePrincipalsByAppId", app_id)
        else file.path("servicePrincipals", object_id)

        call_graph_endpoint(self$token, self$tenant, op)
    }
))
