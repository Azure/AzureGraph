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
#' - `password`: The app password. Note that the Graph API does not return previously-generated passwords. This field will only be populated for an app object created with `ms_graph$create_app()`, or after a call to the `add_password()` method below.
#' @section Methods:
#' - `new(...)`: Initialize a new app object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an app. By default, ask for confirmation first.
#' - `update(...)`: Update the app data in Azure Active Directory. For what properties can be updated, consult the REST API documentation link below.
#' - `do_operation(...)`: Carry out an arbitrary operation on the app.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `list_owners(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)`: Return a list of all owners of this app. Specify the `type` argument to limit the result to specific object type(s).
#' - `create_service_principal(...)`: Create a service principal for this app, by default in the current tenant.
#' - `get_service_principal()`: Get the service principal for this app.
#' - `delete_service_principal(confirm=TRUE)`: Delete the service principal for this app. By default, ask for confirmation first.
#' - `add_password(password_name=NULL, password_duration=NULL)`: Adds a strong password. `password_duration` is the length of time in years that the password remains valid, with default duration 2 years. Returns the ID of the generated password.
#' - `remove_password(password_id, confirm=TRUE)`: Removes the password with the given ID. By default, ask for confirmation first.
#' - `add_certificate(certificate)`: Adds a certificate for authentication. This can be specified as the name of a .pfx or .pem file, an `openssl::cert` object, an `AzureKeyVault::stored_cert` object, or a raw or character vector.
#' - `remove_certificate(certificate_id, confirm=TRUE`): Removes the certificate with the given ID. By default, ask for confirmation first.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_app` and `get_app` methods of the [ms_graph] class. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual app.
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @section List methods:
#' All `list_*` methods have `filter` and `n` arguments to limit the number of results. The former should be an [OData expression](https://docs.microsoft.com/en-us/graph/query-parameters#filter-parameter) as a string to filter the result set on. The latter should be a number setting the maximum number of (filtered) results to return. The default values are `filter=NULL` and `n=Inf`. If `n=NULL`, the `ms_graph_pager` iterator object is returned instead to allow manual iteration over the results.
#'
#' Support in the underlying Graph API for OData queries is patchy. Not all endpoints that return lists of objects support filtering, and if they do, they may not allow all of the defined operators. If your filtering expression results in an error, you can carry out the operation without filtering and then filter the results on the client side.
#' @seealso
#' [ms_graph], [az_service_principal], [az_user], [az_group], [az_object]
#'
#' @examples
#' \dontrun{
#'
#' gr <- get_graph_login()
#' app <- gr$create_app("MyNewApp")
#'
#' # password resetting: remove the old password, add a new one
#' pwd_id <- app$properties$passwordCredentials[[1]]$keyId
#' app$add_password()
#' app$remove_password(pwd_id)
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
#' # add a certificate from a .pem file
#' app$add_certificate("cert.pem")
#'
#' # can also read the file into an openssl object, and then add the cert
#' cert <- openssl::read_cert("cert.pem")
#' app$add_certificate(cert)
#'
#' # add a certificate stored in Azure Key Vault
#' vault <- AzureKeyVault::key_vault("mytenant")
#' cert2 <- vault$certificates$get("certname")
#' app$add_certificate(cert2)
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
        private$api_type <- "applications"
        self$password <- password
        super$initialize(token, tenant, properties)
    },

    add_password=function(password_name=NULL, password_duration=NULL)
    {
        creds <- list()
        if(!is.null(password_name))
            creds$displayName <- password_name
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
        invisible(res$keyId)
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
        self$sync_fields()
        invisible(NULL)
    },

    add_certificate=function(certificate)
    {
        key <- read_cert(certificate)
        creds <- c(self$properties$keyCredentials, list(list(
            key=key,
            type="AsymmetricX509Cert",
            usage="verify"
        )))

        self$update(keyCredentials=creds)
    },

    remove_certificate=function(certificate_id, confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- sprintf("Do you really want to remove the certificate '%s'?", certificate_id)
            if(!get_confirmation(msg, FALSE))
                return(invisible(NULL))
        }

        creds <- self$properties$keyCredentials
        idx <- vapply(creds, function(keycred) keycred$keyId == certificate_id, logical(1))
        if(!any(idx))
            stop("Certificate not found", call.=FALSE)

        self$update(keyCredentials=creds[-idx])
    },

    list_owners=function(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)
    {
        opts <- list(`$filter`=filter, `$count`=if(!is.null(filter)) "true")
        hdrs <- if(!is.null(filter)) httr::add_headers(consistencyLevel="eventual")
        pager <- self$get_list_pager(self$do_operation("owners", options=opts, hdrs, type_filter=type))
        extract_list_values(pager, n)
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
