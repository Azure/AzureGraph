#' Pager object for Graph list results
#'
#' Class representing an _iterator_ for a set of paged query results.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `value_name, next_link_name`: The names of the REST API response components containing the set of values for the page, and the link to the next page. The default values are `value` and `@odata.nextLink` respectively.
#' - `output`: What the pager should yield on each iteration. Either "list", meaning a (nested) list with one component for each individual item in the page; "data.frame", meaning the list output is transformed into a tabular format; or "object", meaning a list of R6 objects as defined by the AzureGraph class framework.
#' - `type_filter`: An optional vector of Graph object types to filter the results on. Only used if `output` is "object".
#' - `init_args`: Any extra arguments required to initialise the returned objects. Only used if `output` is "object".
#' @section Methods:
#' - `new(...)`: Initialize a new user object. See 'Initialization' below.
#' - `has_data()`: Returns TRUE if there are pages remaining in the iterator, or FALSE otherwise.
#' @section Active bindings:
#' - `value`: The returned value on each iteration of the pager.
#'
#' @section Initialization:
#' The recommended way to create objects of this class is via the `ms_object$get_
#' @export
ms_graph_pager <- R6::R6Class("ms_graph_pager",

public=list(

    token=NULL,
    value_name=NULL,
    next_link_name=NULL,
    output=NULL,
    type_filter=NULL,
    init_args=NULL,

    initialize=function(token, first_page, next_link_name="@odata.nextLink", value_name="value",
                        generate_objects=TRUE, type_filter=NULL, ...)
    {
        self$token <- token
        self$value_name <- value_name
        self$next_link_name <- next_link_name
        self$type_filter <- type_filter
        self$init_args <- list(...)
        self$output <- if(is.data.frame(first_page$value))
            "data.frame"
        else if(generate_objects)
            "object"
        else "list"
        private$next_link <- first_page[[next_link_name]]
        private$next_value <- first_page[[value_name]]
    },

    has_data=function()
    {
        !is_empty(private$next_value)
    }
),

active=list(

    value=function()
    {
        val <- private$next_value
        private$next_value <- if(!is.null(private$next_link))
        {
            page <- call_graph_url(self$token, private$next_link, simplify=(self$output == "data.frame"))
            private$next_link <- page[[self$next_link_name]]
            page[[self$value_name]]
        }
        else NULL

        if(self$output == "object")
            private$make_objects(val)
        else val
    }
),

private=list(

    next_link=NULL,
    next_value=NULL,

    make_objects=function(page)
    {
        if(is_empty(page))
            return(NULL)

        page <- lapply(page, function(obj)
        {
            class_gen <- find_class_generator(obj, self$type_filter)
            if(is.null(class_gen))
                NULL
            else do.call(class_gen$new, c(list(self$token, self$tenant, obj), self$init_args))
        })
        page[!sapply(page, is.null)]
    }
))


#' @export
extract_list_values <- function(pager, n=Inf)
{
    if(is.null(n))
        return(pager)

    if(!pager$has_data())
        stop("Pager is empty", call.=FALSE)

    bind_fn <- if(pager$output != "data.frame")
        base::c
    else if(requireNamespace("vctrs", quietly=TRUE))
        vctrs::vec_rbind
    else base::rbind

    res <- NULL
    while(pager$has_data() && NROW(res) < n)  # not nrow()
        res <- bind_fn(res, pager$value)

    if(NROW(res) > n)
        utils::head(res, n)
    else res
}
