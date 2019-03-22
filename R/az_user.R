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
#' - `list_group_memberships()`: Return the IDs of all groups this user is a member of.
#' - `list_object_memberships()`: Return the IDs of all groups, administrative units and directory roles this user is a member of.
#' - `list_direct_memberships(id_only=TRUE)`: List the groups this user is a direct member of. Set `id_only=TRUE` to return only a vector of group IDs (the default), or `id_only=FALSE` to return a list of group objects.
#' - `reset_password(password=NULL, force_password_change=TRUE): Resets a user password. By default the new password will be randomly generated, and must be changed at next login.
#'
#' @section Initialization:
#' Creating new objects of this class should be done via the `create_user` and `get_user` methods of the [ms_graph] and [az_app] classes. Calling the `new()` method for this class only constructs the R object; it does not call the Microsoft Graph API to create the actual user account.
#'
#' @seealso
#' [ms_graph], [az_app], [az_group], [az_object]
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
#' head(grps)
#'
#' }
#' @format An R6 object of class `az_user`, inheriting from `az_object`.
#' @export
az_user <- R6::R6Class("az_user", inherit=az_object,

public=list(

    password=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$type <- "user"
        self$password <- password
        super$initialize(token, tenant, properties)
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
        self$graph_op(op, body=properties, encode="json", http_verb="PATCH")
        self$properties <- self$graph_op(op)
        self$password <- password
        password
    },

    list_direct_memberships=function(id_only=TRUE)
    {
        op <- file.path("users", self$properties$id, "memberOf")
        res <- get_paged_list(self$graph_op(op), self$token)

        if(id_only)
            sapply(res, function(grp) grp$id)
        else
        {
            names(res) <- sapply(res, function(grp) grp$displayName)
            lapply(res, function(grp) az_group$new(self$token, self$tenant, grp))
        }
    },

    print=function(...)
    {
        cat("<Graph user account '", self$properties$displayName, "'>\n", sep="")
        cat("  principal name:", self$properties$userPrincipalName, "\n")
        cat("  email:", self$properties$mail, "\n")
        cat("  directory id:", self$properties$id, "\n")
        invisible(self)
    }
))
