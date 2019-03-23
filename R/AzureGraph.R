#' @import AzureAuth
#' @importFrom utils modifyList
NULL

utils::globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    options(azure_graph_api_version="beta")
    make_AzureR_dir()
    invisible(NULL)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

