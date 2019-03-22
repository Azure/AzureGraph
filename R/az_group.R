#' Group in Azure Active Directory
#'
#' Base class representing an AAD group.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this group.
#' - `properties`: The group properties.
#' @section Methods:
#' - `new(...)`: Initialize a new group object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a group. By default, ask for confirmation first.
#' - `update(...)`: Update the group information in Azure Active Directory.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `list_group_memberships()`: Return the IDs of all groups this group is a member of.
#' - `list_object_memberships()`: Return the IDs of all groups, administrative units and directory roles this group is a member of.
#' - `list_members()`: Return a list of all members of this group.
#' - `list_owners()`: Return a list of all owners of this group.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_group` and `get_group` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual group.
#'
#' @seealso
#' [ms_graph], [az_app], [az_user], [az_object]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @examples
#' \dontrun{
#'
#' gr <- get_graph_login()
#' usr <- gr$get_user("myname@aadtenant.com")
#'
#' grps <- usr$list_direct_memberships()
#' grp <- grp[[1]]
#'
#' grp$list_members()
#' grp$list_owners()
#'
#' }
#' @format An R6 object of class `az_group`, inheriting from `az_object`.
#' @export
az_group <- R6::R6Class("az_group", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "group"
        super$initialize(token, tenant, properties)
    },

    list_members=function()
    {
        op <- file.path("groups", self$properties$id, "members")
        lst <- self$graph_op(op)
        res <- get_paged_list(lst, self$token)

        lapply(res, function(obj)
        {
            if(obj$`@odata.type` == "#microsoft.graph.user")
                az_user$new(self$token, self$tenant, obj)
            else if(obj$`@odata.type` == "#microsoft.graph.group")
                az_group$new(self$token, self$tenant, obj)
            else if(obj$`@odata.type` == "#microsoft.graph.application")
                az_app$new(self$token, self$tenant, obj)
            else if(obj$`@odata.type` == "#microsoft.graph.servicePrincipal")
                az_service_principal$new(self$token, self$tenant, obj)
            else
            {
                warning("Unknown directory object type ", obj$`@odata.type`)
                obj
            }
        })
    },

    list_owners=function()
    {
        op <- file.path("groups", self$properties$id, "owners")
        res <- self$graph_op(op)$value
        lapply(res, function(obj) az_user$new(self$token, self$tenant, obj))
    },

    print=function(...)
    {
        cat("<Graph group '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        invisible(self)
    }
))
