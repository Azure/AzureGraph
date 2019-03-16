az_user <- R6::R6Class("az_user",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, properties=NULL, password=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
        private$password <- password
    },

    reset_password=function(password=NULL, temporary=TRUE)
    {
        if(is.null(password))
        {
            chars <- c(letters, LETTERS, 0:9, "~", "!", "@", "#", "$", "%", "&", "*", "(", ")", "-", "+")
            password <- paste0(sample(chars, 50, replace=TRUE), collapse="")
        }

        properties <- list(
            passwordProfile=list(
                password=password,
                forceChangeAtNextLogin=temporary
            )
        )

        op <- file.path("users", self$properties$objectId)
        private$graph_op(op, body=properties, encode="json", http_verb="PATCH")
        private$graph_op(op)
        private$password <- password
        password
    },

    update=function(...)
    {
        properties=list(...)
        op <- file.path("users", self$properties$objectId)
        private$graph_op(op, body=list(...), encode="json", http_verb="PATCH")
        private$graph_op(op)
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

        op <- file.path("users", self$properties$objectId)
        private$graph_op(op, http_verb="DELETE")
        invisible(NULL)
    }
),

private=list(

    password=NULL,

    graph_op=function(op="", ...)
    {
        call_graph_endpoint(self$token, self$tenant, op, ...)
    }
))
