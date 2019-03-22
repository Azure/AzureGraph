#' User in Azure Active Directory
#'
#' Base class representing an AAD user account.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this user.
#' - `properties`: The user properties.
#' @section Methods:
#' - `new(...)`: Initialize a new user object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a user account. By default, ask for confirmation first.
#' - `update(...)`: Update the user information in Azure Active Directory.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#' - `reset_password(password=NULL, force_password_change=TRUE): Resets a user password. By default the new password will be randomly generated, and must be changed at next login.
#' - `list_group_memberships(direct_only=TRUE, id_only=TRUE)`: List the groups this user is a member of. Set `direct_only=FALSE` to get a _transitive_ list of memberships, ie including groups that the user's groups are members of. Set `id_only=TRUE` to return only a vector of group IDs (the default), or `id_only=FALSE` to return a list of group objects (which will be slow for a transitive list).
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_user` and `get_user` methods of the [gr_graph] and [gr_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual user account.
#'
#' @seealso
#' [gr_graph], [gr_app], [gr_group]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-beta)
#'
#' @format An R6 object of class `gr_user`.
#' @export
gr_user <- R6::R6Class("gr_user",

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

    list_group_memberships=function(direct_only=TRUE, id_only=TRUE)
    {
        res <- if(direct_only)
            private$list_direct_memberships(id_only)
        else private$list_transitive_memberships(id_only)

        if(!id_only)
        {
            names(res) <- sapply(res, function(grp) grp$displayName)
            lapply(res, function(grp) gr_group$new(self$token, self$tenant, grp))
        }
        else unlist(res)
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
    },

    print=function(...)
    {
        cat("<Graph user account '", self$properties$displayName, "'>\n", sep="")
        cat("  principal name:", self$properties$userPrincipalName, "\n")
        cat("  email:", self$properties$mail, "\n")
        cat("  directory id:", self$properties$id, "\n")
        invisible(self)
    }
),

private=list(

    list_transitive_memberships=function(id_only)
    {
        op <- file.path("users", self$properties$id, "getMemberGroups")
        lst <- private$graph_op(op, body=list(securityEnabledOnly=TRUE),
            encode="json", http_verb="POST")

        res <- lst$value
        while(!is_empty(lst$`@odata.nextLink`))
        {
            lst <- call_graph_url(self$token, lst$`@odata.nextLink`)
            res <- c(res, lst$value)
        }

        if(!id_only)
        {
            lapply(res, function(grp)
            {
                op <- file.path("groups", grp)
                private$graph_op(op)
            })
        }
        else res
    },

    list_direct_memberships=function(id_only)
    {
        op <- file.path("users", self$properties$id, "memberOf")
        lst <- private$graph_op(op)

        res <- lst$value
        while(!is_empty(lst$`@odata.nextLink`))
        {
            lst <- call_graph_url(self$token, lst$`@odata.nextLink`)
            res <- c(res, lst$value)
        }

        if(id_only)
            lapply(res, function(grp) grp$id)
        else res
    },

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, op, ...)
    }
))
