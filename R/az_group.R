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
#' - `list_group_memberships()`: Return the IDs of all groups this group is a member of.
#' - `list_object_memberships()`: Return the IDs of all groups, administrative units and directory roles this group is a member of.
#' - `list_members(type=c("user", "group", "application", "servicePrincipal"))`: Return a list of all members of this group. Specify the `type` argument to filter the result for specific object type(s).
#' - `list_owners(type=c("user", "group", "application", "servicePrincipal"))`: Return a list of all owners of this group. Specify the `type` argument to filter the result for specific object type(s).
#' - `get_sharepoint_site()`: Get the SharePoint site associated with this group, if it exists.
#' - `list_drives()`: List the drives (shared document libraries) associated with this group.
#' - `get_drive()`: Get the default document library for this group.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_group` and `get_group` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual group.
#'
#' @seealso
#' [ms_graph], [az_app], [az_user], [az_object]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
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
        private$api_type <- "groups"
        super$initialize(token, tenant, properties)
    },

    list_members=function(type=c("user", "group", "application", "servicePrincipal"))
    {
        res <- private$get_paged_list(self$do_operation("members"))
        private$init_list_objects(res, type)
    },

    list_owners=function(type=c("user", "group", "application", "servicePrincipal"))
    {
        res <- private$get_paged_list(self$do_operation("owners"))
        private$init_list_objects(res, type)
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
