#' Directory role
#'
#' Class representing a role in Azure Active Directory.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this role.
#' - `type`: always "directory role" for a directory role object.
#' - `properties`: The item properties.
#' @section Methods:
#' - `new(...)`: Initialize a new object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete this item. By default, ask for confirmation first.
#' - `update(...)`: Update the item's properties in Microsoft Graph.
#' - `do_operation(...)`: Carry out an arbitrary operation on the item.
#' - `sync_fields()`: Synchronise the R object with the item metadata in Microsoft Graph.
#'
#' @section Initialization:
#' Currently support for directory roles is limited. Objects of this class should not be initialized directly.
#'
#' @seealso
#' [ms_graph], [az_user]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @format An R6 object of class `az_directory_role`, inheriting from `az_object`.
#' @export
az_directory_role <- R6::R6Class("az_directory_role", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "directory role"
        private$api_type <- "directoryRoles"
        super$initialize(token, tenant, properties)
    },

    list_members=function(n=Inf)
    {
        pager <- self$get_list_pager(self$do_operation("members"))
        get_list_values(pager, n)
    },

    print=function(...)
    {
        cat("<Azure Active Directory role '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
