#' Group in Azure Active Directory
#'
#' Class representing an AAD group.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this group.
#' - `type`: always "group" for a group object.
#' - `properties`: The group properties.
#' @section Methods:
#' - `new(...)`: Initialize a new group object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a group. By default, ask for confirmation first.
#' - `update(...)`: Update the group information in Azure Active Directory.
#' - `do_operation(...)`: Carry out an arbitrary operation on the group.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `list_members(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)`: Return a list of all members of this group. Specify the `type` argument to limit the result to specific object type(s).
#' - `list_owners(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)`: Return a list of all owners of this group. Specify the `type` argument to limit the result to specific object type(s).
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_group` and `get_group` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual group.
#'
#' @section List methods:
#' All `list_*` methods have `filter` and `n` arguments to limit the number of results. The former should be an [OData expression](https://learn.microsoft.com/en-us/graph/query-parameters#filter-parameter) as a string to filter the result set on. The latter should be a number setting the maximum number of (filtered) results to return. The default values are `filter=NULL` and `n=Inf`. If `n=NULL`, the `ms_graph_pager` iterator object is returned instead to allow manual iteration over the results.
#'
#' Support in the underlying Graph API for OData queries is patchy. Not all endpoints that return lists of objects support filtering, and if they do, they may not allow all of the defined operators. If your filtering expression results in an error, you can carry out the operation without filtering and then filter the results on the client side.
#' @seealso
#' [ms_graph], [az_app], [az_user], [az_object]
#'
#' [Microsoft Graph overview](https://learn.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @examples
#' \dontrun{
#'
#' gr <- get_graph_login()
#' usr <- gr$get_user("myname@aadtenant.com")
#'
#' grps <- usr$list_group_memberships()
#' grp <- gr$get_group(grps[1])
#'
#' grp$list_members()
#' grp$list_owners()
#'
#' # capping the number of results
#' grp$list_members(n=10)
#'
#' # get the pager object for a listing method
#' pager <- grp$list_members(n=NULL)
#' pager$value
#'
#' }
#' @format An R6 object of class `az_group`, inheriting from `az_object`.
#' @export
az_group <- R6::R6Class("az_group", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "group"
        private$api_type <- "groups"
        super$initialize(token, tenant, properties)
    },

    list_members=function(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)
    {
        opts <- list(`$filter`=filter, `$count`=if(!is.null(filter)) "true")
        hdrs <- if(!is.null(filter)) httr::add_headers(consistencyLevel="eventual")
        pager <- self$get_list_pager(self$do_operation("members", options=opts, hdrs), type_filter=type)
        extract_list_values(pager, n)
    },

    list_owners=function(type=c("user", "group", "application", "servicePrincipal"), filter=NULL, n=Inf)
    {
        opts <- list(`$filter`=filter, `$count`=if(!is.null(filter)) "true")
        hdrs <- if(!is.null(filter)) httr::add_headers(consistencyLevel="eventual")
        pager <- self$get_list_pager(self$do_operation("owners", options=opts, hdrs), type_filter=type)
        extract_list_values(pager, n)
    },

    print=function(...)
    {
        group_type <- if("Unified" %in% self$properties$groupTypes)
            "Microsoft 365"
        else if(!self$properties$mailEnabled)
            "Security"
        else if(self$properties$securityEnabled)
            "Mail-enabled security"
        else "Distribution"
        cat("<", group_type, " group '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
