#' Azure Active Directory object
#'
#' Base class representing a object in Microsoft Graph. All other Graph object classes ultimately inherit from this class.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this object.
#' - `type`: The type of object, in a human-readable format.
#' - `properties`: The object properties.
#' @section Methods:
#' - `new(...)`: Initialize a new directory object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an object. By default, ask for confirmation first.
#' - `update(...)`: Update the object information in Azure Active Directory.
#' - `do_operation(...)`: Carry out an arbitrary operation on the object.
#' - `sync_fields()`: Synchronise the R object with the data in Azure Active Directory.
#'
#' @section Initialization:
#' Objects of this class should not be created directly. Instead, create an object of the appropriate subclass.
#'
#' @seealso
#' [ms_graph], [az_object]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://docs.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @format An R6 object of class `ms_object`.
#' @export
ms_object <- R6::R6Class("ms_object",

public=list(

    token=NULL,
    tenant=NULL,

    # user-readable object type
    type=NULL,

    # object data from server
    properties=NULL,

    # any additional data
    extra=list(),

    initialize=function(token, tenant=NULL, properties=NULL, ...)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
        self$extra <- list(...)
    },

    update=function(...)
    {
        self$do_operation(body=list(...), encode="json", http_verb="PATCH")
        self$properties <- self$do_operation()
        self
    },

    sync_fields=function()
    {
        self$properties <- self$do_operation()
        invisible(self)
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            name <- self$properties$displayName
            if(is.null(name))
                name <- self$properties$name
            if(is.null(name))
                name <- self$properties$id
            msg <- sprintf("Do you really want to delete the %s '%s'?",
                           self$type, name)
            if(!get_confirmation(msg, FALSE))
                return(invisible(NULL))
        }

        self$do_operation(http_verb="DELETE")
        invisible(NULL)
    },

    do_operation=function(op="", ...)
    {
        op <- construct_path(private$api_type, self$properties$id, op)
        call_graph_endpoint(self$token, op, ...)
    },

    print=function(...)
    {
        cat("<Graph directory object '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
),

private=list(

    # object type as it appears in REST API path
    api_type=NULL,

    get_paged_list=function(lst, next_link_name="@odata.nextLink", value_name="value", simplify=FALSE, n=Inf)
    {
        bind_fn <- if(requireNamespace("vctrs"))
            vctrs::vec_rbind
        else base::rbind
        res <- lst[[value_name]]
        if(n <= 0) n <- Inf
        while(!is_empty(lst[[next_link_name]]) && length(res) < n)
        {
            lst <- call_graph_url(self$token, lst[[next_link_name]], simplify=simplify)
            res <- if(simplify)
                bind_fn(res, lst[[value_name]])  # this assumes all objects have the exact same fields
            else c(res, lst[[value_name]])
        }
        if(n < length(res))
            res[seq_len(n)]
        else res
    },

    init_list_objects=function(lst, type_filter=NULL, default_generator=ms_object, ...)
    {
        lst <- lapply(lst, function(obj)
        {
            class_gen <- find_class_generator(obj, type_filter, default_generator)
            if(is.null(class_gen))
                NULL
            else class_gen$new(self$token, self$tenant, obj, ...)
        })
        lst[!sapply(lst, is.null)]
    }
))

