#' Login to Azure Active Directory Graph
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID. The default is to login via the "common" tenant, which will infer your actual tenant from your credentials.
#' @param app The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but you can supply your own app credentials as well.
#' @param password If `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' @param username If `auth_type == "resource_owner"`, your username.
#' @param certificate If `auth_type == "client_credentials", a certificate to authenticate with. This is a more secure alternative to using an app secret.
#' @param auth_type The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". If `NULL`, this is chosen based on the presence of the `username` and `password` arguments.
#' @param version The Azure Active Directory version to use for authenticating.
#' @param host Your Microsoft Graph host. Defaults to `https://graph.microsoft.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param scopes The Microsoft Graph scopes (permissions) to obtain for this Graph login. Only for `version=2`.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param token Optionally, an OAuth 2.0 token, of class [AzureAuth::AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, all other arguments to `create_graph_login` will be ignored.
#' @param refresh For `get_graph_login`, whether to refresh the authentication token on loading the client.
#' @param selection For `get_graph_login`, if you have multiple logins for a given tenant, which one to use. This can be a number, or the input MD5 hash of the token used for the login. If not supplied, `get_graph_login` will print a menu and ask you to choose a login.
#' @param confirm For `delete_graph_login`, whether to ask for confirmation before deleting.
#' @param ... Other arguments passed to `ms_graph$new()`.
#'
#' @details
#' `create_graph_login` creates a login client to authenticate with Microsoft Graph, using the supplied arguments. The authentication token is obtained using [get_azure_token], which automatically caches and reuses tokens for subsequent sessions. Note that credentials are only cached if you allowed AzureGraph to create a data directory at package startup.
#'
#' `get_graph_login` returns a previously created login client. If there are multiple existing clients, you can specify which client to return via the `selection`, `app`, `scopes` and `auth_type` arguments. If you don't specify which one to return, it will pop up a menu and ask you to choose one.
#'
#' One difference between `create_graph_login` and `get_graph_login` is the former will delete any previously saved credentials that match the arguments it was given. You can use this to force AzureGraph to remove obsolete tokens that may be lying around.
#'
#' @section Linux DSVM note:
#' If you are using a Linux [Data Science Virtual Machine](https://azure.microsoft.com/en-us/services/virtual-machines/data-science-virtual-machines/) in Azure, you may have problems running `create_graph_login()` (ie, without any arguments). In this case, try `create_graph_login(auth_type="device_code")`.
#'
#' @return
#' For `get_graph_login` and `create_graph_login`, an object of class `ms_graph`, representing the login client. For `list_graph_logins`, a (possibly nested) list of such objects.
#'
#' If the AzureR data directory for saving credentials does not exist, `get_graph_login` will throw an error.
#'
#' @seealso
#' [ms_graph], [AzureAuth::get_azure_token] for more details on authentication methods
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @examples
#' \dontrun{
#'
#' # without any arguments, this will create a client using your AAD organisational account
#' az <- create_graph_login()
#'
#' # retrieve the login in subsequent sessions
#' az <- get_graph_login()
#'
#' # this will create an Microsoft Graph client for the tenant 'microsoft.onmicrosoft.com',
#' # using the client_credentials method
#' az <- create_graph_login("mytenant", app="{app_id}", password="{password}")
#'
#' # you can also login using credentials in a json file
#' az <- create_graph_login(config_file="~/creds.json")
#'
#' # creating and obtaining a login with specific scopes
#' create_graph_login("mytenant", scopes=c("User.Read", "Files.ReadWrite.All"))
#' get_graph_login("mytenant", scopes=c("User.Read", "Files.ReadWrite.All"))
#'
#' # to use your personal account, set the tenant to one of the following
#' create_graph_login("9188040d-6c67-4c5b-b112-36a304b66dad")
#' create_graph_login("consumers")  # requires AzureAuth 1.2.6
#'
#' }
#' @rdname graph_login
#' @export
create_graph_login <- function(tenant="common", app=NULL,
                               password=NULL, username=NULL, certificate=NULL, auth_type=NULL, version=2,
                               host="https://graph.microsoft.com/", aad_host="https://login.microsoftonline.com/",
                               scopes=".default", config_file=NULL, token=NULL, ...)
{
    if(!is_azure_token(token))
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            call <- as.list(match.call())[-1]
            call$config_file <- call$token <- NULL
            call <- lapply(modifyList(call, conf), function(x) eval.parent(x))
            return(do.call(create_graph_login, call))
        }

        tenant <- normalize_tenant(tenant)
        app <- if(is.null(app))
        {
            if(tenant %in% c("consumers", "9188040d-6c67-4c5b-b112-36a304b66dad"))
                .azurer_graph_app_id
            else .az_cli_app_id
        }
        else normalize_guid(app)

        if(version == 2)
            host <- c(paste0(host, scopes), "openid", "offline_access")

        token_args <- list(resource=host,
            tenant=tenant,
            app=app,
            password=password,
            username=username,
            certificate=certificate,
            auth_type=auth_type,
            aad_host=aad_host,
            version=version,
            ...)

        hash <- do.call(token_hash, token_args)
        tokenfile <- file.path(AzureR_dir(), hash)
        if(file.exists(tokenfile))
        {
            message("Deleting existing Azure Active Directory token for this set of credentials")
            file.remove(tokenfile)
        }

        message("Creating Microsoft Graph login for ", format_tenant(tenant))
        token <- do.call(get_azure_token, token_args)
    }
    else tenant <- token$tenant

    client <- ms_graph$new(token=token)

    # save login info for future sessions
    graph_logins <- load_graph_logins()
    graph_logins[[tenant]] <- sort(unique(c(graph_logins[[tenant]], client$token$hash())))
    save_graph_logins(graph_logins)

    client
}


#' @rdname graph_login
#' @export
get_graph_login <- function(tenant="common", selection=NULL, app=NULL, scopes=NULL, auth_type=NULL, refresh=TRUE)
{
    if(!dir.exists(AzureR_dir()))
        stop("AzureR data directory does not exist; cannot load saved logins")

    tenant <- normalize_tenant(tenant)

    graph_logins <- load_graph_logins()
    this_login <- graph_logins[[tenant]]
    if(is_empty(this_login))
    {
        msg <- paste0("No Microsoft Graph logins found for ", format_tenant(tenant),
                      ";\nuse create_graph_login() to create one")
        stop(msg, call.=FALSE)
    }

    message("Loading Microsoft Graph login for ", format_tenant(tenant))

    # do we need to choose which login client to use?
    have_selection <- !is.null(selection)
    have_auth_spec <- any(!is.null(app), !is.null(scopes), !is.null(auth_type))

    token <- if(length(this_login) > 1 || have_selection || have_auth_spec)
        choose_token(this_login, selection, app, scopes, auth_type)
    else load_azure_token(this_login)

    if(is.null(token))
        return(NULL)

    client <- ms_graph$new(token=token)
    if(refresh)
        client$token$refresh()
    client
}


#' @rdname graph_login
#' @export
delete_graph_login <- function(tenant="common", confirm=TRUE)
{
    if(!dir.exists(AzureR_dir()))
    {
        warning("AzureR data directory does not exist; no logins to delete")
        return(invisible(NULL))
    }

    tenant <- normalize_tenant(tenant)

    if(confirm && interactive())
    {
        msg <- paste0("Do you really want to delete the Microsoft Graph login(s) for ",
                      format_tenant(tenant), "?")
        if(!get_confirmation(msg, FALSE))
            return(invisible(NULL))
    }

    graph_logins <- load_graph_logins()
    graph_logins[[tenant]] <- NULL
    save_graph_logins(graph_logins)
    invisible(NULL)
}


#' @rdname graph_login
#' @export
list_graph_logins <- function()
{
    graph_logins <- load_graph_logins()
    logins <- sapply(graph_logins, function(tenant)
    {
        sapply(tenant, function(hash)
        {
            file <- file.path(AzureR_dir(), hash)
            ms_graph$new(token=readRDS(file))
        }, simplify=FALSE)
    }, simplify=FALSE)

    logins
}


load_graph_logins <- function()
{
    file <- file.path(AzureR_dir(), "graph_logins.json")
    if(!file.exists(file))
        return(named_list())
    jsonlite::fromJSON(file)
}


save_graph_logins <- function(logins)
{
    if(!dir.exists(AzureR_dir()))
    {
        message("AzureR data directory does not exist; login credentials not saved")
        return(invisible(NULL))
    }

    if(is_empty(logins))
        names(logins) <- character(0)

    file <- file.path(AzureR_dir(), "graph_logins.json")
    writeLines(jsonlite::toJSON(logins, auto_unbox=TRUE, pretty=TRUE), file)
    invisible(NULL)
}


format_tenant <- function(tenant)
{
    if(tenant == "common")
        "default tenant"
    else paste0("tenant '", tenant, "'")
}


# algorithm for choosing a token:
# if given a hash, choose it (error if no match)
# otherwise if given any of app|scopes|auth_type, use those (error if no match)
# otherwise if given a number, use it
# otherwise ask
choose_token <- function(hashes, selection, app, scopes, auth_type)
{
    if(is.character(selection))
    {
        if(!(selection %in% hashes))
            stop("Token with selected hash not found", call.=FALSE)
        return(load_azure_token(selection))
    }

    if(any(!is.null(app), !is.null(scopes), !is.null(auth_type)))
    {
        if(!is.null(scopes))
            scopes <- tolower(scopes)

        # look for matching token
        for(hash in hashes)
        {
            app_match <- scope_match <- auth_match <- TRUE
            token <- load_azure_token(hash)
            if(!is.null(app) && token$client$client_id != hash)
                app_match <- FALSE
            if(!is.null(scopes))
            {
                # is this an AAD v1.0 token?
                if(is.null(token$scope))
                    scope_match <- FALSE
                else
                {
                    tok_scopes <- tolower(basename(grep("^.+://", token$scope, value=TRUE)))
                    if(!setequal(scopes, tok_scopes))
                        scope_match <- FALSE
                }
            }
            if(!is.null(auth_type) && token$auth_type != auth_type)
                auth_match <- FALSE

            if(app_match && scope_match && auth_match)
                return(token)
        }
        # if we get here, no tokens match provided criteria
        stop("Token with selected authentication parameters not found", call.=FALSE)
    }

    if(is.numeric(selection))
    {
        if(selection > length(hashes))
            stop("Invalid numeric selection", call.=FALSE)
        return(load_azure_token(hashes[selection]))
    }

    # bring up a menu
    tokens <- lapply(hashes, load_azure_token)
    tenant <- tokens[[1]]$tenant
    choices <- sapply(tokens, function(token)
    {
        app <- token$client$client_id
        scopes <- if(!is.null(token$scope))
            paste(tolower(basename(grep("^.+://", token$scope, value=TRUE))), collapse=" ")
        else "<NA>"
        paste0("App ID: ", app,
               "\n   Scopes: ", scopes,
               "\n   Authentication method: ", token$auth_type,
               "\n   MD5 Hash: ", token$hash())
    })
    msg <- paste0("Choose a Microsoft Graph login for ", format_tenant(tenant))
    selection <- utils::menu(choices, title=msg)
    if(selection == 0)
        NULL
    else tokens[[selection]]
}

