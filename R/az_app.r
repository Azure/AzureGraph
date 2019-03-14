#' Registered app in Azure Active Directory
#'
#' Base class representing an AAD app.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new app object. See 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an app. By default, ask for confirmation first.
#' - `update(...)`: Update the app data in Azure Active Directory.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `create_service_principal(...)`: Create a service principal for this app, by default in the current tenant.
#' - `get_service_principal()`: Get the service principal for this app.
#' - `delete_service_principal(confirm=TRUE)`: Delete the service principal for this app. By default, ask for confirmation first.
#'
#' @section Initialization:
#' The recommended way to create a new app object is via the `create_app` and `get_app` methods of the [az_graph] class, which will handle the details automatically.
#'
#' [Azure AD Graph overview](https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-graph-api),
#' [REST API reference](https://docs.microsoft.com/en-au/previous-versions/azure/ad/graph/api/api-catalog)
#'
#' @seealso
#' [az_graph], [az_service_principal]
#'
#' @format An R6 object of class `az_app`.
#' @export
az_app <- R6::R6Class("az_app",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
        private$password <- password
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the '", self$properties$displayName, "' app? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- if(!is.null(self$properties$objectId))
            file.path("applications", self$properties$objectId)
        else file.path("applicationsByAppId", self$properties$appId)

        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    },

    update_password=function(...)
    {},

    update=function(...)
    {},

    sync_fields=function()
    {
        op <- file.path("applications", self$properties$objectId)
        self$properties <- call_graph_endpoint(self$token, self$tenant, op)
        invisible(self)
    },

    create_service_principal=function(...)
    {
        properties <- modifyList(list(...), list(appId=self$properties$appId))
        az_service_principal$new(
            self$token,
            self$tenant,
            private$graph_op("servicePrincipals", body=properties, encode="json", http_verb="POST")
        )
    },

    get_service_principal=function()
    {
        op <- file.path("servicePrincipalsByAppId", self$properties$appId)
        az_service_principal$new(
            self$token,
            self$tenant,
            private$graph_op(op)
        )
    },

    delete_service_principal=function(confirm=TRUE)
    {
        self$get_service_principal()$delete(confirm=confirm)
    }
),

private=list(

    password=NULL,

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, self$tenant, op, ...)
    }
))
