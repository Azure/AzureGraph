#' Pager object for Graph list results
#'
#' Class representing an _iterator_ for a set of paged query results.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `output`: What the pager should yield on each iteration, either "data.frame","list" or "object". See 'Value' below.
#' @section Methods:
#' - `new(...)`: Initialize a new user object. See 'Initialization' below.
#' - `has_data()`: Returns TRUE if there are pages remaining in the iterator, or FALSE otherwise.
#' @section Active bindings:
#' - `value`: The returned value on each iteration of the pager.
#'
#' @section Initialization:
#' The recommended way to create objects of this class is via the `ms_object$get_list_pager()` method, but it can also be initialized directly. The arguments to the `new()` method are:
#' - `token`: The token used to authenticate with the Graph host.
#' - `first_page`: A list containing the first page of results, generally from a call to `call_graph_endpoint()` or the `do_operation()` method of an AzureGraph R6 object.
#' - `next_link_name,value_name`: The names of the components of `first_page` containing the link to the next page, and the set of values for the page respectively. The default values are `@odata.nextLink` and `value`.
#' - `generate_objects`: Whether the iterator should return a list containing the parsed JSON for the page values, or convert it into a list of R6 objects. See 'Value' below.
#' - `type_filter`: Any extra arguments required to initialise the returned objects. Only used if `generate_objects` is TRUE.
#' - `...`: Any extra arguments required to initialise the returned objects. Only used if `generate_objects` is TRUE.
#'
#' @section Value:
#' The `value` active binding returns the page values for each iteration of the pager. This can take one of 3 forms, based on the initial format of the first page and the `generate_objects` argument.
#'
#' If the first page of results is a data frame (each item has been converted into a row), then the pager will return results as data frames. In this case, the `output` field is automatically set to "data.frame" and the `generate_objects` initialization argument is ignored. Usually this will be the case when the results are meant to represent external data, eg items in a SharePoint list.
#'
#' If the first page of results is a list, the `generate_objects` argument sets whether to convert the items in each page into R6 objects defined by the AzureGraph class framework. If `generate_objects` is TRUE, the `output` field is set to "object", and if `generate_objects` is FALSE, the `output` field is set to "list".
#'
#' @seealso
#' [ms_object], [extract_list_values]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [Paging documentation](https://docs.microsoft.com/en-us/graph/paging)
#'
#' @examples
#' \dontrun{
#'
#' # list direct memberships
#' firstpage <- call_graph_endpoint(token, "me/memberOf")
#'
#' pager <- ms_graph_pager$new(token, firstpage)
#' pager$has_data()
#' pager$value
#'
#' # once all the pages have been returned
#' isFALSE(pager$has_data())
#' is.null(pager$value)
#'
#' # returning items, 1 per page, as raw lists of properties
#' firstpage <- call_graph_endpoint(token, "me/memberOf", options=list(`$top`=1))
#' pager <- ms_graph_pager$new(token, firstpage, generate_objects=FALSE)
#' lst <- NULL
#' while(pager$has_data())
#'     lst <- c(lst, pager$value)
#'
#' # returning items as a data frame
#' firstdf <- call_graph_endpoint(token, "me/memberOf", options=list(`$top`=1),
#'                                simplify=TRUE)
#' pager <- ms_graph_pager$new(token, firstdf)
#' df <- NULL
#' while(pager$has_data())
#'     df <- vctrs::vec_rbin(df, pager$value)
#'
#' }
#' @format An R6 object of class `ms_graph_pager`.
#' @export
ms_graph_pager <- R6::R6Class("ms_graph_pager",

public=list(

    token=NULL,
    output=NULL,

    initialize=function(token, first_page, next_link_name="@odata.nextLink", value_name="value",
                        generate_objects=TRUE, type_filter=NULL, ...)
    {
        self$token <- token
        private$value_name <- value_name
        private$next_link_name <- next_link_name
        private$type_filter <- type_filter
        private$init_args <- list(...)
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
        !is.null(private$next_value)
    }
),

active=list(

    value=function()
    {
        val <- private$next_value
        private$next_value <- if(!is.null(private$next_link))
        {
            page <- call_graph_url(self$token, private$next_link, simplify=(self$output == "data.frame"))
            private$next_link <- page[[private$next_link_name]]
            page[[private$value_name]]
        }
        else NULL

        if(self$output == "object")
            private$make_objects(val)
        else val
    }
),

private=list(

    next_link_name=NULL,
    value_name=NULL,
    next_link=NULL,
    next_value=NULL,
    type_filter=NULL,
    init_args=NULL,

    make_objects=function(page)
    {
        if(is_empty(page))
            return(list())

        page <- lapply(page, function(obj)
        {
            class_gen <- find_class_generator(obj, private$type_filter)
            if(is.null(class_gen))
                NULL
            else do.call(class_gen$new, c(list(self$token, self$tenant, obj), private$init_args))
        })
        page[!sapply(page, is.null)]
    }
))


#' Get the list of values from a Graph pager object
#'
#' @param pager An object of class `ms_graph_pager`, which is an iterator for a list of paged query results.
#' @param n The number of items from the list to return. Note this is _not_ the number of _pages_ (each page will usually contain multiple items). The default value of `Inf` extracts all the values from the list, leaving the pager empty. If this is NULL, the pager itself is returned.
#'
#' @details
#' This is a convenience function to perform the common task of extracting all or some of the items from a paged response.
#'
#' @return
#' If `n` is `Inf` or a number, the items from the paged query results. The format of the returned value depends on the pager settings. This will either be a raw list containing the properties for each of the items; a list of R6 objects; or a data frame. If the pager is empty, the returned value will be NULL.
#'
#' If `n` is NULL, the pager itself is returned.
#'
#' @seealso
#' [ms_graph_pager], [ms_object], [call_graph_endpoint]
#'
#' @examples
#' \dontrun{
#'
#' firstpage <- call_graph_endpoint(token, "me/memberOf")
#' pager <- ms_graph_pager$new(token, firstpage)
#' extract_list_values(pager)
#'
#' # trying to extract values a 2nd time returns NULL
#' is.null(extract_list_values(pager))
#'
#' }
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
