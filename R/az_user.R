#' User in Azure Active Directory
#'
#' Base class representing an AAD user account.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new user object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a user account. By default, ask for confirmation first.
#' - `update(...)`: Update the user information in Azure Active Directory.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `reset_password(password=NULL, force_password_change=TRUE): Resets a user password. By default the new password will be randomly generated, and must be changed at next login.
#' - `list_group_memberships()`: List the groups this user is a member of.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_user` and `get_user` methods of the [az_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual user account.
#'
#' @seealso
#' [az_graph], [az_app], [az_group]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @format An R6 object of class `az_user`.
#' @export
az_user <- R6::R6Class("az_user",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    password=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
        self$password <- password
    },

    reset_password=function(password=NULL, force_password_change=TRUE)
    {
        if(is.null(password))
            password <- openssl::base64_encode(openssl::rand_bytes(40))

        properties <- modifyList(properties, list(
            passwordProfile=list(
                password=password,
                forceChangePasswordNextSignIn=force_password_change,
                forceChangePasswordNextSignInWithMfa=FALSE
            )
        ))

        op <- file.path("users", self$properties$id)
        private$graph_op(op, body=properties, encode="json", http_verb="PATCH")
        self$properties <- private$graph_op(op)
        self$password <- password
        password
    },

    update=function(...)
    {
        op <- file.path("users", self$properties$id)
        private$graph_op(op, body=list(...), encode="json", http_verb="PATCH")
        self$properties <- private$graph_op(op)
        self
    },

    sync_fields=function()
    {
        op <- file.path("users", self$properties$id)
        self$properties <- private$graph_op(op)
        invisible(self)
    },

    list_group_memberships=function(transitive=FALSE)
    {
        op <- file.path("users", self$properties$id,
            if(transitive) "getMemberGroups" else "memberOf")
        lst <- private$graph_op(op)

        res <- lapply(lst$value, function(obj) az_group$new(self$token, self$tenant, obj))
        while(!is_empty(lst$`@odata.nextLink`))
        {
            lst <- call_graph_url(self$token, lst$`@odata.nextLink`)
            res <- c(res,
                lapply(lst$value, function(obj) az_group$new(self$token, self$tenant, obj)))
        }

        names(res) <- sapply(res, function(grp) grp$properties$displayName)
        res
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the user '", self$properties$displayName,
                          "'? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("users", self$properties$id)
        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    }
),

private=list(

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, self$tenant, op, ...)
    }
))
