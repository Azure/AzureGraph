#' Microsoft Graph object
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
#' - `get_list_pager(...)`: Returns a pager object, which is an _iterator_ for a set of paged query results. See 'Paged results' below.
#'
#' @section Initialization:
#' Objects of this class should not be created directly. Instead, create an object of the appropriate subclass.
#'
#' @section List methods:
#' All `list_*` methods have `filter` and `n` arguments to limit the number of results. The former should be an [OData expression](https://docs.microsoft.com/en-us/graph/query-parameters#filter-parameter) as a string to filter the result set on. The latter should be a number setting the maximum number of (filtered) results to return. The default values are `filter=NULL` and `n=Inf`. If `n=NULL`, the `ms_graph_pager` iterator object is returned instead to allow manual iteration over the results.
#'
#' Support in the underlying Graph API for OData queries is patchy. Not all endpoints that return lists of objects support filtering, and if they do, they may not allow all of the defined operators. If your filtering expression results in an error, you can carry out the operation without filtering and then filter the results on the client side.
#'
#' @section Paged results:
#' Microsoft Graph returns lists in pages, with each page containing a subset of objects and a link to the next page. AzureGraph provides an iterator-based API that lets you access each page individually, or collect them all into a single object.
#'
#' To create a new pager object, call the `get_list_pager()` method with the following arguments:
#' - `lst`: A list containing the first page of results, generally from a call to the `do_operation()` method.
#' - `next_link_name,value_name`: The names of the components of `first_page` containing the link to the next page, and the set of values for the page respectively. The default values are `@odata.nextLink` and `value`.
#' - `generate_objects`: Whether the iterator should return a list containing the parsed JSON for the page values, or convert it into a list of R6 objects.
#' - `type_filter`: Any extra arguments required to initialise the returned objects. Only used if `generate_objects` is TRUE.
#' - `...`: Any extra arguments required to initialise the returned objects. Only used if `generate_objects` is TRUE.
#'
#' This returns an object of class [ms_graph_pager], which is an _iterator_ for the set of paged results. Each call to the object's `value` active binding yields the next page. When all pages have been returned, `value` contains NULL.
#'
#' The format of the returned values can take one of 3 forms, based on the initial format of the first page and the `generate_objects` argument.
#'
#' If the first page of results is a data frame (each item has been converted into a row), then the pager will return results as data frames. In this case, the `output` field is automatically set to "data.frame" and the `generate_objects` initialization argument is ignored. Usually this will be the case when the results are meant to represent external data, eg items in a SharePoint list.
#'
#' If the first page of results is a list, the `generate_objects` argument sets whether to convert the items in each page into R6 objects defined by the AzureGraph class framework. If `generate_objects` is TRUE, the `output` field is set to "object", and if `generate_objects` is FALSE, the `output` field is set to "list".
#'
#' You can also call the `extract_list_values()` function to get all or some of the values from a pager, without having to manually combine the pages together.
#'
#' @section Deprecated methods:
#' The following methods are private and **deprecated**, and form the older AzureGraph API for accessing paged results. They will eventually be removed.
#' - `get_paged_list(lst, next_link_name, value_name, simplify, n)`: This method reconstructs the list, given the first page.
#' - `init_list_objects(lst, type_filter, default_generator, ...)`: `get_paged_list` returns a raw list, the result of parsing the JSON response from the Graph host. This method converts the list into actual R6 objects.
#'
#' @seealso
#' [ms_graph], [az_object], [ms_graph_pager], [extract_list_values]
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

    get_list_pager=function(lst, next_link_name="@odata.nextLink", value_name="value", generate_objects=TRUE,
                            type_filter=NULL, ...)
    {
        ms_graph_pager$new(self$token, lst, next_link_name, value_name, generate_objects, type_filter, ...)
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
        while(!is_empty(lst[[next_link_name]]) && NROW(res) < n)
        {
            lst <- call_graph_url(self$token, lst[[next_link_name]], simplify=simplify)
            res <- if(simplify)
                bind_fn(res, lst[[value_name]])  # base::rbind assumes all objects have the exact same fields
            else c(res, lst[[value_name]])
        }
        if(n < NROW(res))
        {
            if(inherits(res, "data.frame"))
                res[seq_len(n), ]
            else res[seq_len(n)]
        }
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

