#' Login to Azure Active Directory Graph
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID. The default is to login via the "common" tenant, which will infer your actual tenant from your credentials.
#' @param app The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but you can supply your own app credentials as well.
#' @param password If `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' @param username If `auth_type == "resource_owner"`, your username.
#' @param auth_type The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". If `NULL`, this is chosen based on the presence of the `username` and `password` arguments.
#' @param host Your Microsoft Graph host. Defaults to `https://graph.microsoft.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param refresh For `get_graph_login`, whether to refresh the authentication token on loading the client.
#' @param selection For `get_graph_login`, if you have multiple logins for a given tenant, which one to use. This can be a number, or the input MD5 hash of the token used for the login. If not supplied, `get_graph_login` will print a menu and ask you to choose a login.
#' @param confirm For `delete_graph_login`, whether to ask for confirmation before deleting.
#' @param ... Other arguments passed to `ms_graph$new()`.
#'
#' @details
#' `create_graph_login` creates a login client to authenticate with Microsoft Graph, using the supplied arguments. The authentication token is obtained using [get_azure_token], which automatically caches and reuses tokens for subsequent sessions. Note that credentials are only cached if you allowed AzureGraph to create a data directory at package startup.
#'
#' `get_graph_login` returns a login client by retrieving previously saved credentials. It searches for saved credentials according to the supplied tenant; if multiple logins are found, it will prompt for you to choose one.
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
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @examples
#' \dontrun{
#'
#' # without any arguments, this will create a client using your AAD credentials
#' az <- create_graph_login() 
#'
#' # retrieve the login in subsequent sessions
#' az <- get_graph_login()
#'
#' # this will create an Microsoft Graph client for the tenant 'microsoft.onmicrosoft.com',
#' # using the client_credentials method
#' az <- create_graph_login("microsoft", app="{app_id}", password="{password}")
#'
#' # you can also login using credentials in a json file
#' az <- create_graph_login(config_file="~/creds.json")
#'
#' }
#' @rdname graph_login
#' @export
create_graph_login <- function(tenant="common", app=.az_cli_app_id, password=NULL, username=NULL, auth_type=NULL,
                               host="https://graph.microsoft.com/", aad_host="https://login.microsoftonline.com/",
                               config_file=NULL, ...)
{
    if(!is.null(config_file))
    {
        conf <- jsonlite::fromJSON(config_file)
        if(!is.null(conf$tenant)) tenant <- conf$tenant
        if(!is.null(conf$app)) app <- conf$app
        if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
        if(!is.null(conf$password)) password <- conf$password
        if(!is.null(conf$host)) host <- conf$host
        if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
    }

    hash <- token_hash(
        resource=host,
        tenant=tenant,
        app=app,
        password=password,
        username=username,
        auth_type=auth_type,
        aad_host=aad_host
    )
    tokenfile <- file.path(AzureR_dir(), hash)
    if(file.exists(tokenfile))
    {
        message("Deleting existing Azure Active Directory token for this set of credentials")
        file.remove(tokenfile)
    }

    tenant <- normalize_tenant(tenant)
    app <- normalize_guid(app)

    message("Creating Microsoft Graph login for ", format_tenant(tenant))
    client <- ms_graph$new(tenant, app, password, username, auth_type, host, aad_host, config_file, ...)

    # save login info for future sessions
    graph_logins <- load_graph_logins()
    graph_logins[[tenant]] <- sort(unique(c(graph_logins[[tenant]], client$token$hash())))
    save_graph_logins(graph_logins)

    client
}


#' @rdname graph_login
#' @export
get_graph_login <- function(tenant="common", selection=NULL, refresh=TRUE)
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

    if(length(this_login) == 1 && is.null(selection))
        selection <- 1
    else if(is.null(selection))
    {
        tokens <- lapply(this_login, function(f)
            readRDS(file.path(AzureR_dir(), f)))

        choices <- sapply(tokens, function(token)
        {
            app <- token$client$client_id
            paste0("App ID: ", app, "\n   Authentication method: ", token$auth_type)
        })

        msg <- paste0("Choose a Microsoft Graph login for ", format_tenant(tenant))
        selection <- utils::menu(choices, title=msg)
    }

    if(selection == 0)
        return(NULL)

    file <- if(is.numeric(selection))
        this_login[selection]
    else if(is.character(selection))
        this_login[which(this_login == selection)] # force an error if supplied hash doesn't match available logins

    file <- file.path(AzureR_dir(), file)
    if(is_empty(file) || !file.exists(file))
        stop("Azure Active Directory token not found for this login", call.=FALSE)

    message("Loading Microsoft Graph login for ", format_tenant(tenant))

    token <- readRDS(file)
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
                      format_tenant(tenant), "? (y/N) ")

        yn <- readline(msg)
        if(tolower(substr(yn, 1, 1)) != "y")
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