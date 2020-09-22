#' Sites (SharePoint) in Azure Active Directory
#'
#' Base class representing an AAD sites.
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
#' @format An R6 object of class `az_site`, inheriting from `az_object`.
#' @export
az_site <- R6::R6Class("az_site", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "site"
        super$initialize(token, tenant, properties)
    },

    search_drive=function(site_id="ba207007-6b47-4d9e-a415-10d23a18f318,e4ccf907-24df-4b4e-8b25-c03d6d160ccc", drive_name="Documents")
    {
        op <- sprintf("sites/%s/?search=%s", curl::curl_escape(site_id), site_name)

        az_drive$new(self$token, self$tenant, self$call_graph_endpoint(op))
    },

    get_drive=function(site_id="ba207007-6b47-4d9e-a415-10d23a18f318,e4ccf907-24df-4b4e-8b25-c03d6d160ccc", drive_id="b!B3Agukdrnk2kFRDSOhjzGAf5zOTfJE5LiyXAPW0WDMy2nDgqTdhORJXWNGidYJAY")
    {
        op <- file.path("sites", curl::curl_escape(site_id),"drives",curl::curl_escape(drive_id))

        az_drive$new(self$token, self$tenant, self$call_graph_endpoint(op))
    },

    print=function(...)
    {
        cat("<Graph site '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
