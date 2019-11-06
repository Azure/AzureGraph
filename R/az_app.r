#' Registered app in Azure Active Directory
#'
#' Base class representing an AAD app.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this app.
#' - `type`: always "application" for an app object.
#' - `properties`: The app properties.
#' - `password`: The app password. Note that the Graph API does not return passwords, so this will be NULL for an app retrieved via `ms_graph$get_app()`.
#' @section Methods:
#' - `new(...)`: Initialize a new app object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an app. By default, ask for confirmation first.
#' - `update(...)`: Update the app data in Azure Active Directory. For what properties can be updated, consult the REST API documentation link below.
#' - `do_operation(...)`: Carry out an arbitrary operation on the app.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `list_group_memberships()`: Return the IDs of all groups this app is a member of.
#' - `list_object_memberships()`: Return the IDs of all groups, administrative units and directory roles this app is a member of.
#' - `list_owners(type=c("user", "group", "application", "servicePrincipal"))`: Return a list of all owners of this app. Specify the `type` argument to filter the result for specific object type(s).
#' - `create_service_principal(...)`: Create a service principal for this app, by default in the current tenant.
#' - `get_service_principal()`: Get the service principal for this app.
#' - `delete_service_principal(confirm=TRUE)`: Delete the service principal for this app. By default, ask for confirmation first.
#' - `add_password(password_name=NULL, password_duration=NULL)`: Adds a strong password.
#' - `remove_password(password_id, confirm=TRUE)`: Removes the password with the given ID. By default, ask for confirmation first.
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

    add_password=function(password_name=NULL, password_duration=NULL)
    {
        creds <- list()
        if(!is.null(name))
            creds$displayName <- name
        if(!is.null(password_duration))
        {
            now <- as.POSIXlt(Sys.time())
            now$year <- now$year + password_duration
            creds$endDateTime <- format(as.POSIXct(now), "%Y-%m-%dT%H:%M:%SZ", tz="GMT")
        }

        properties <- if(!is_empty(creds))
            list(passwordCredential=creds)
        else NULL

        res <- self$do_operation("addPassword", body=properties, http_verb="POST")
        self$properties <- self$do_operation()
        self$password <- res$secretText
        invisible(self$password)
    },

    remove_password=function(password_id, confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- sprintf("Do you really want to remove the password '%s'?", password_id)
            if(!get_confirmation(msg, FALSE))
                return(invisible(NULL))
        }

        self$do_operation("removePassword", body=list(keyId=password_id), http_verb="POST")
        invisible(NULL)
    },

    list_owners=function(type=c("user", "group", "application", "servicePrincipal"))
    {
        res <- private$get_paged_list(self$do_operation("owners"))
        private$init_list_objects(private$filter_list(res, type))
    },

    create_service_principal=function(...)
    {
        properties <- modifyList(list(...), list(appId=self$properties$appId))
        az_service_principal$new(
            self$token,
            self$tenant,
            call_graph_endpoint(self$token, "servicePrincipals", body=properties, encode="json", http_verb="POST")
        )
    },

    get_service_principal=function()
    {
        op <- sprintf("servicePrincipals?$filter=appId+eq+'%s'", self$properties$appId)
        az_service_principal$new(
            self$token,
            self$tenant,
            call_graph_endpoint(self$token, op)$value[[1]]
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
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
