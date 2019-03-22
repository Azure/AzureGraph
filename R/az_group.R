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
#' - `list_members()`: Return a list of all members of this group.
#' - `list_owners()`: Return a list of all owners of this group.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_group` and `get_group` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual group.
#'
#' @seealso
#' [ms_graph], [az_app], [az_user]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @format An R6 object of class `az_group`.
#' @export
az_group <- R6::R6Class("az_group",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
    },

    update=function(...)
    {
        op <- file.path("groups", self$properties$id)
        self$graph_op(op, body=list(...), encode="json", http_verb="PATCH")
        self$properties <- self$graph_op(op)
        self
    },

    sync_fields=function()
    {
        op <- file.path("groups", self$properties$id)
        self$properties <- self$graph_op(op)
        invisible(self)
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the group '", self$properties$displayName,
                          "'? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("groups", self$properties$id)
        self$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    },

    list_members=function()
    {
        op <- file.path("groups", self$properties$id, "members")
        lst <- self$graph_op(op)

        res <- lst$value
        while(!is_empty(lst$`@odata.nextLink`))
        {
            lst <- call_graph_url(self$token, lst$`@odata.nextLink`)
            res <- c(res, lst$value)
        }

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
        lst <- self$graph_op(op)

        res <- lst$value
        lapply(res, function(obj) az_user$new(self$token, self$tenant, obj))
    },

    print=function(...)
    {
        cat("<Graph group '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        invisible(self)
    },

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    }
))
