#' @import AzureAuth
#' @importFrom utils modifyList
NULL

utils::globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    options(azure_graph_api_version="v1.0")
    invisible(NULL)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

# authentication app ID for personal accounts
.azurer_graph_app_id <- "5bb21e8a-06bf-4ac4-b613-110ac0e582c1"
