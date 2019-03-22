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
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_group` and `get_group` methods of the [gr_graph] and [gr_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual group.
#'
#' @seealso
#' [gr_graph], [gr_app], [gr_user]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @format An R6 object of class `gr_group`.
#' @export
gr_group <- R6::R6Class("gr_group",

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
        private$graph_op(op, body=list(...), encode="json", http_verb="PATCH")
        self$properties <- private$graph_op(op)
        self
    },

    sync_fields=function()
    {
        op <- file.path("groups", self$properties$id)
        self$properties <- private$graph_op(op)
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
        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    },
    
    print=function(...)
    {
        cat("<Graph group '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  description:", self$properties$description, "\n")
        invisible(self)
    }
),

private=list(

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    }
))
