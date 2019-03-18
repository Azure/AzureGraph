#' Registered app in Azure Active Directory
#'
#' Base class representing an AAD app.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new app object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an app. By default, ask for confirmation first.
#' - `update(...)`: Update the app data in Azure Active Directory. For what properties can be updated, consult the REST API documentation link below.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `create_service_principal(...)`: Create a service principal for this app, by default in the current tenant.
#' - `get_service_principal()`: Get the service principal for this app.
#' - `delete_service_principal(confirm=TRUE)`: Delete the service principal for this app. By default, ask for confirmation first.
#' - `update_password(password=NULL, name="key1", password_duration=1)`: Updates the app password. Note that this will invalidate any existing password.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_app` and `get_app` methods of the [az_graph] class. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual app.
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @seealso
#' [az_graph], [az_service_principal], [az_user], [az_group]
#'
#' @examples
#' \dontrun{
#'
#' gr <- get_graph_login()
#' app <- gr$create_app("MyNewApp")
#'
#' # password reset
#' app$update_password()
#'
#' # set a redirect URI
#' app$update(replyUrls=I("http://localhost:1410"))
#'
#' # change the app name
#' app$update(displayName="MyRenamedApp")
#'
#' }
#' @format An R6 object of class `az_app`.
#' @export
az_app <- R6::R6Class("az_app",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    password=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
        self$password <- password
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the app '", self$properties$displayName, "'? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("applications", self$properties$id)
        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    },

    update_password=function(password=NULL, name="key1", password_duration=1)
    {
        key <- openssl::base64_encode(iconv(name, to="UTF-16LE", toRaw=TRUE)[[1]])
        if(is.null(password))
            password <- openssl::base64_encode(openssl::rand_bytes(40))

        end_date <- if(is.finite(password_duration))
        {
            now <- as.POSIXlt(Sys.time())
            now$year <- now$year + password_duration
            format(as.POSIXct(now), "%Y-%m-%dT%H:%M:%SZ", tz="GMT")
        }
        else "2299-12-30T12:00:00Z"

        properties <- list(
            passwordCredentials=list(list(
                customKeyIdentifier=key,
                endDateTime=end_date,
                secretText=password
            ))
        )

        op <- file.path("applications", self$properties$id)
        private$graph_op(op, body=properties, encode="json", http_verb="PATCH")
        self$properties <- private$graph_op(op)
        self$password <- password
        password
    },

    update=function(...)
    {
        op <- file.path("applications", self$properties$id)
        private$graph_op(op, body=list(...), encode="json", http_verb="PATCH")
        self$properties <- private$graph_op(op)
        self
    },

    sync_fields=function()
    {
        op <- file.path("applications", self$properties$id)
        self$properties <- private$graph_op(op)
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
        op <- sprintf("servicePrincipals?$filter=appId+eq+'%s'", self$properties$appId)
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

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    }
))
