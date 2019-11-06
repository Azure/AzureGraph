#' Azure Active Directory Graph
#'
#' Base class for interacting with Microsoft Graph API.
#'
#' @docType class
#' @section Methods:
#' - `new(tenant, app, ...)`: Initialize a new Microsoft Graph connection with the given credentials. See 'Authentication' for more details.
#' - `create_app(name, ..., add_password=TRUE, password_name=NULL, password_duration=2, certificate=NULL, create_service_principal=TRUE)`: Creates a new registered app in Azure Active Directory. See 'App creation' below.
#' - `get_app(app_id, object_id)`: Retrieves an existing registered app, via either its app ID or object ID.
#' - `delete_app(app_id, object_id, confirm=TRUE)`: Deletes an existing registered app. Any associated service principal will also be deleted.
#' - `create_service_principal(app_id, ...)`: Creates a service principal for a registered app.
#' - `get_service_principal()`: Retrieves an existing service principal.
#' - `delete_service_principal()`: Deletes an existing service principal.
#' - `create_user(name, email, enabled=TRUE, ..., password=NULL, force_password_change=TRUE)`: Creates a new user account. By default this will be a work account (not social or local) in the current tenant, and will have a randomly generated password that must be changed at next login.
#' - `get_user(user_id)`: Retrieves an existing user account.
#' - `delete_user(user_id, confirm=TRUE)`: Deletes a user account.
#' - `create_group(name, email, ...)`: Creates a new group. Note that only security groups can be created via the Microsoft Graph API.
#' - `get_group(group_id)`: Retrieves an existing group.
#' - `delete_group(group_id, confirm=TRUE)`: Deletes a group.
#' - `call_graph_endpoint(op="", ...)`: Calls the Microsoft Graph API using this object's token and tenant as authentication arguments. See [call_graph_endpoint].
#'
#' @section Authentication:
#' The recommended way to authenticate with Microsoft Graph is via the [create_graph_login] function, which creates a new instance of this class.
#'
#' To authenticate with the `ms_graph` class directly, provide the following arguments to the `new` method:
#' - `tenant`: Your tenant ID. This can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' - `app`: The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but it's recommended to supply your own app credentials if possible.
#' - `password`: if `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' - `username`: if `auth_type == "resource_owner"`, your username.
#' - `certificate`: If `auth_type == "client_credentials", a certificate to authenticate with. This is a more secure alternative to using an app secret.
#' - `auth_type`: The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". See [get_azure_token] for how the default method is chosen, along with some caveats.
#' - `host`: your Microsoft Graph host. Defaults to `https://graph.microsoft.com/`.
#' - `aad_host`: Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' - `token`: Optionally, an OAuth 2.0 token, of class [AzureAuth::AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, all other arguments will be ignored.
#'
#' @section App creation:
#' The `create_app` method creates a new registered app. By default, a new app will have a randomly generated strong password with duration of 2 years. To skip assigning a password, set the `add_password` argument to FALSE.
#'
#' The `certificate` argument allows authenticating via a certificate instead of a password. This should be a character string containing the certificate public key (aka the CER file). Alternatively it can be an list, or an object of class `AzureKeyVault::stored_cert` representing a certificate stored in an Azure Key Vault. See the examples below.
#'
#' A new app will also have a service principal created for it by default. To disable this, set `create_service_principal=FALSE`.
#'
#' @seealso
#' [create_graph_login], [get_graph_login]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @examples
#' \dontrun{
#'
#' # start a new Graph session
#' gr <- ms_graph$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")
#'
#' # authenticate with credentials in a file
#' gr <- ms_graph$new(config_file="creds.json")
#'
#' # authenticate with device code
#' gr <- ms_graph$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", auth_type="device_code")
#'
#' # retrieve a registered app
#' gr$get_app(app_id="myappid")
#'
#' # create a new app and associated service principal, set password duration to 10 years
#' app <- gr$create_app("mynewapp", password_duration=10)
#'
#' # delete the app
#' gr$delete_app(app_id=app$properties$appId)
#' # ... but better to call the object's delete method directly
#' app$delete()
#'
#' # create an app with authentication via a certificate
#' cert <- readLines("mycert.cer")
#' gr$create_app("mycertapp", password=FALSE, certificate=cert)
#'
#' }
#' @format An R6 object of class `ms_graph`.
#' @export
ms_graph <- R6::R6Class("ms_graph",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant="common", app=.az_cli_app_id,
                        password=NULL, username=NULL, certificate=NULL, auth_type=NULL,
                        host="https://graph.microsoft.com/", aad_host="https://login.microsoftonline.com/",
                        token=NULL, ...)
    {
        if(is_azure_token(token))
        {
            self$host <- httr::build_url(find_resource_host(token))
            self$tenant <- token$tenant
            self$token <- token
            return(NULL)
        }

        self$host <- host
        self$tenant <- normalize_tenant(tenant)
        app <- normalize_guid(app)

        self$token <- get_azure_token(resource=self$host,
            tenant=self$tenant,
            app=app,
            password=password,
            username=username,
            certificate=certificate,
            auth_type=auth_type,
            aad_host=aad_host,
            ...)

        NULL
    },

    create_app=function(name, ..., add_password=TRUE, password_name=NULL, password_duration=2, certificate=NULL,
        create_service_principal=TRUE)
    {
        properties <- list(displayName=name, ...)
        if(!is_empty(certificate))
        {
            key <- if(is.character(certificate))
                certificate
            else if(is.list(certificate) || inherits(certificate, "stored_cert"))
                certificate$cer

            properties <- modifyList(properties, list(
                keyCredentials=list(list(
                    key=key,
                    type="AsymmetricX509Cert",
                    usage="verify"
                ))
            ))
        }

        res <- az_app$new(
            self$token,
            self$tenant,
            self$call_graph_endpoint("applications", body=properties, encode="json", http_verb="POST"),
            password
        )

        if(create_service_principal)
            res$create_service_principal()
        if(add_password)
            res$add_password(password_name, password_duration)
        res
    },

    get_app=function(app_id=NULL, object_id=NULL)
    {
        if(!is.null(app_id))
        {
            op <- sprintf("applications?$filter=appId+eq+'%s'", app_id)
            az_app$new(self$token, self$tenant, self$call_graph_endpoint(op)$value[[1]])
        }
        else if(!is.null(object_id))
        {
            op <- file.path("applications", object_id)
            az_app$new(self$token, self$tenant, self$call_graph_endpoint(op))
        }
        else stop("Must supply either app ID or object (directory) ID")
    },

    delete_app=function(app_id=NULL, object_id=NULL, confirm=TRUE)
    {
        self$get_app(app_id, object_id)$delete(confirm=confirm)
    },

    create_service_principal=function(app_id, ...)
    {
        self$get_app(app_id)$create_service_principal(...)
    },

    get_service_principal=function(app_id=NULL, object_id=NULL)
    {
        if(!is.null(app_id) && is_guid(app_id))
        {
            op <- sprintf("servicePrincipals?$filter=appId+eq+'%s'", app_id)
            az_service_principal$new(self$token, self$tenant, self$call_graph_endpoint(op)$value[[1]])
        }
        else if(!is.null(object_id) && is_guid(object_id))
        {
            op <- file.path("servicePrincipals", object_id)
            az_service_principal$new(self$token, self$tenant, self$call_graph_endpoint(op))
        }
        else stop("Must supply either app ID or object (directory) ID")
    },

    delete_service_principal=function(app_id=NULL, object_id=NULL, confirm=TRUE)
    {
        self$get_service_principal(app_id, object_id)$delete(confirm=confirm)
    },

    create_user=function(name, email, enabled=TRUE, ..., password=NULL, force_password_change=TRUE)
    {
        properties <- list(
            displayName=name,
            mail=email,
            userPrincipalName=email,
            mailNickname=email,
            accountEnabled=enabled
        )

        properties <- modifyList(properties, list(...))

        if(is.null(password))
            password <- openssl::base64_encode(openssl::rand_bytes(40))

        properties <- modifyList(properties, list(
            passwordProfile=list(
                password=password,
                forceChangePasswordNextSignIn=force_password_change,
                forceChangePasswordNextSignInWithMfa=FALSE
            )
        ))

        az_user$new(
            self$token,
            self$tenant,
            self$call_graph_endpoint("users", body=properties, encode="json", http_verb="POST"),
            password
        )
    },

    get_user=function(user_id="me")
    {
        op <- if(user_id == "me")
            "me"
        else file.path("users", curl::curl_escape(user_id))

        az_user$new(self$token, self$tenant, self$call_graph_endpoint(op))
    },

    delete_user=function(user_id, confirm=TRUE)
    {
        self$get_user(user_id)$delete(confirm=confirm)
    },

    create_group=function(name, email, ...)
    {
        properties <- list(
            displayName=name,
            mailEnabled=FALSE,
            mailNickname=email,
            securityEnabled=TRUE
        )

        az_group$new(
            self$token,
            self$tenant,
            self$call_graph_endpoint("groups", body=properties, encode="json", http_verb="POST")
        )
    },

    get_group=function(group_id)
    {
        op <- file.path("groups", group_id)
        az_group$new(self$token, self$tenant, self$call_graph_endpoint(op))
    },

    delete_group=function(group_id, confirm=TRUE)
    {
        self$get_group(group_id)$delete(confirm=confirm)
    },

    call_graph_endpoint=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    },

    print=function(...)
    {
        cat("<Microsoft Graph client>\n")
        cat("<Authentication>\n")
        fmt_token <- gsub("\n  ", "\n    ", format_auth_header(self$token))
        cat(" ", fmt_token)
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))

