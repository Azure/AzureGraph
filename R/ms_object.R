#' Azure Active Directory object
#'
#' Base class representing a object in Microsoft Graph. All other Graph object classes ultimately inherit from this class.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this object.
#' - `type`: The type of object, in a human-readable format.
#' - `properties`: The object properties, as obtained from the Graph host.
#' @section Methods:
#' - `new(...)`: Initialize a new directory object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete an object. By default, ask for confirmation first.
#' - `update(...)`: Update the object information in Azure Active Directory.
#' - `do_operation(...)`: Carry out an arbitrary operation on the object.
#' - `sync_fields()`: Synchronise the R object with the data in Azure Active Directory.
#'
#' The following methods are private, and are intended for package authors extending AzureGraph to define their own objects.
#' - `get_paged_list(lst, next_link_name, value_name, simplify, n)`: Used to process API calls that return lists of objects. Microsoft Graph returns lists in pages, with each page containing a subset of objects and a link to the next page. This method reconstructs the list, given the first page. Its arguments are:
#'    - `lst`: Object containing the initial page of results, generally the result of a call to `do_operation`. Should be a list with names corresponding to the arguments `next_link_name` and `value_name`.
#'    - `next_link_name`: The name of the component of `lst` containing the link to the next page. Defaults to `@odata.nextLink`.
#'    - `value_name`: The name of the component of `lst` containing the first page of results. Defaults to `value`.
#'    - `simplify`: Whether to turn the list of objects into a data frame. This is useful if the list is intended to be a rectangular data structure, eg a SharePoint list or OneDrive file listing. Note that the vctrs package must be installed to return a data frame, if the objects in the list can have varying structures (which will often be the case).
#'    - `n`: Optionally, limit the list to this many objects.
#' - `init_list_objects(lst, type_filter, default_generator, ...)`: `get_paged_list` returns a raw list, the result of parsing the JSON response from the Graph host. This method converts the list into actual R6 objects. Its arguments are:
#'   - `lst`: The input list.
#'   - `type_filter`: The possible types of objects that the list contains. The default is NULL.
#'   - `default_generator`: An R6 class generator object to use, if `init_list_objects` is unable to detect the type of an object. It's recommended to change this from the default value of `ms_object`, if you know that all objects in the list will have the same class.
#'   - `...`: Further arguments to pass to the generator's `initialize` method.
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

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$token <- token
        self$tenant <- tenant
        self$properties <- properties
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
        bind_fn <- if(requireNamespace("vctrs", quietly=TRUE))
            vctrs::vec_rbind
        else base::rbind
        res <- lst[[value_name]]
        if(n <= 0) n <- Inf
        while(!is_empty(lst[[next_link_name]]) && length(res) < n)
        {
            lst <- call_graph_url(self$token, lst[[next_link_name]], simplify=simplify)
            res <- if(simplify)
                bind_fn(res, lst[[value_name]])  # base::rbind assumes all objects have the exact same fields
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

