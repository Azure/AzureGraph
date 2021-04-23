#' Service principal in Azure Active Directory
#'
#' Class representing an AAD service principal.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this service principal.
#' - `type`: always "service principal" for a service principal object.
#' - `properties`: The service principal properties.
#' @section Methods:
#' - `new(...)`: Initialize a new service principal object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a service principal. By default, ask for confirmation first.
#' - `update(...)`: Update the service principal information in Azure Active Directory.
#' - `do_operation(...)`: Carry out an arbitrary operation on the service principal.
#' - `sync_fields()`: Synchronise the R object with the service principal data in Azure Active Directory.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_service_principal` and `get_service_principal` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual service principal.
#'
#' @seealso
#' [ms_graph], [az_app], [az_object]
#'
#' [Azure Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @format An R6 object of class `az_service_principal`, inheriting from `az_object`.
#' @export
az_service_principal <- R6::R6Class("az_service_principal", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "service principal"
        private$api_type <- "servicePrincipals"
        super$initialize(token, tenant, properties)
    },

    print=function(...)
    {
        cat("<Graph service principal '", self$properties$displayName, "'>\n", sep="")
        cat("  app id:", self$properties$appId, "\n")
        cat("  directory id:", self$properties$id, "\n")
        cat("  app tenant:", self$properties$appOwnerOrganizationId, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
