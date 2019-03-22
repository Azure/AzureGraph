#' Registered app in Azure Active Directory
#'
#' Base class representing an AAD app.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this app.
#' - `properties`: The app properties.
#' - `password`: The app password. Note that the Graph API does not return passwords, so this will be NULL for an app retrieved via `ms_graph$get_app()`.
#' @section Methods:
#' - `new(...)`: Initialize a new app object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an app. By default, ask for confirmation first.
#' - `update(...)`: Update the app data in Azure Active Directory. For what properties can be updated, consult the REST API documentation link below.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `list_group_memberships()`: Return the IDs of all groups this app is a member of.
#' - `list_object_memberships()`: Return the IDs of all groups, administrative units and directory roles this app is a member of.
#' - `create_service_principal(...)`: Create a service principal for this app, by default in the current tenant.
#' - `get_service_principal()`: Get the service principal for this app.
#' - `delete_service_principal(confirm=TRUE)`: Delete the service principal for this app. By default, ask for confirmation first.
#' - `update_password(password=NULL, name="key1", password_duration=1)`: Updates the app password. Note that this will invalidate any existing password.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_app` and `get_app` methods of the [ms_graph] class. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual app.
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @seealso
#' [ms_graph], [az_service_principal], [az_user], [az_group], [az_object]
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
#' app$update(publicClient=list(redirectUris=I("http://localhost:1410")))
#'
#' # add API permission (access Azure Storage as user)
#' app$update(requiredResourceAccess=list(
#'     list(
#'         resourceAppId="e406a681-f3d4-42a8-90b6-c2b029497af1",
#'         resourceAccess=list(
#'             list(
#'                 id="03e0da56-190b-40ad-a80c-ea378c433f7f",
#'                 type="Scope"
#'             )
#'         )
#'     )
#' ))
#'
#' # change the app name
#' app$update(displayName="MyRenamedApp")
#'
#' }
#' @format An R6 object of class `az_app`, inheriting from `az_object`.
#' @export
az_app <- R6::R6Class("az_app", inherit=az_object,

public=list(

    password=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$type <- "application"
        self$password <- password
        super$initialize(token, tenant, properties)
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
        self$graph_op(op, body=properties, encode="json", http_verb="PATCH")
        self$properties <- self$graph_op(op)
        self$password <- password
        password
    },

    create_service_principal=function(...)
    {
        properties <- modifyList(list(...), list(appId=self$properties$appId))
        az_service_principal$new(
            self$token,
            self$tenant,
            self$graph_op("servicePrincipals", body=properties, encode="json", http_verb="POST")
        )
    },

    get_service_principal=function()
    {
        op <- sprintf("servicePrincipals?$filter=appId+eq+'%s'", self$properties$appId)
        az_service_principal$new(
            self$token,
            self$tenant,
            self$graph_op(op)$value[[1]]
        )
    },

    delete_service_principal=function(confirm=TRUE)
    {
        self$get_service_principal()$delete(confirm=confirm)
    },

    print=function(...)
    {
        cat("<Graph registered app '", self$properties$displayName, "'>\n", sep="")
        cat("  app id:", self$properties$appId, "\n")
        cat("  directory id:", self$properties$id, "\n")
        cat("  domain:", self$properties$publisherDomain, "\n")
        invisible(self)
    }
))
