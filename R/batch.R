#' Microsoft Graph request
#'
#' Class representing a request to the Microsoft Graph API. Currently this is used only in building a batch call.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a new request object with the given parameters. See 'Details' below.
#' - `batchify()`: Generate a list object suitable for incorporating into a call to the batch endpoint.
#' @section Details:
#' The `initialize()` method takes the following arguments, representing the components of a HTTPS request:
#' - `op`: The path of the HTTPS URL, eg `/me/drives`.
#' - `body`: The body of the HTTPS request, if it is a PUT, POST or PATCH.
#' - `options`: A list containing the query parameters for the URL.
#' - `headers`: Any optional HTTP headers for the request.
#' - `encode`: If a request body is present, how it should be encoded when sending it to the endpoint. The default is `json`, meaning it will be sent as JSON text; an alternative is `raw`, for binary data.
#' - `http_verb`: One of "GET" (the default), "DELETE", "PUT", "POST", "HEAD", or "PATCH".
#'
#' This class is currently used only for building batch calls. Future versions of AzureGraph may be refactored to use it in general API calls as well.
#' @seealso
#' [call_batch_endpoint]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [Batch endpoint documentation](https://docs.microsoft.com/en-us/graph/json-batching)
#'
#' @examples
#' graph_request$new("me")
#'
#' # a new email message in Outlook
#' graph_request$new("me/messages",
#'     body=list(
#'         body=list(
#'             content="Hello from R",
#'             content_type="text"
#'         ),
#'         subject="Hello",
#'         toRecipients="bob@example.com"
#'     ),
#'     http_verb="POST"
#' )
#' @format An R6 object of class `graph_request`.
#' @export
graph_request <- R6::R6Class("graph_request",

public=list(
    method=NULL,
    op=NULL,
    options=list(),
    headers=list(),
    body=NULL,
    encode=NULL,

    initialize=function(op, body=NULL, options=list(), headers=list(), encode="json",
                        http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD", "PATCH"))
    {
        self$op <- op
        self$method <- match.arg(http_verb)
        self$options <- options
        self$headers <- headers
        self$body <- body
        self$encode <- encode
    },

    batchify=function()
    {
        url <- httr::parse_url("foo://bar")  # dummy scheme and host
        url$path <- self$op
        url$query <- self$options
        url <- httr::build_url(url)
        req <- list(
            id=NULL,
            method=self$method,
            url=substr(url, 10, nchar(url))
        )
        hdrs <- self$headers
        if(!is_empty(self$body))
        {
            hdrs$`Content-Type` <- if(self$encode == "json")
                "application/json"
            else if(self$encode == "raw")
                "application/octet-stream"
            else self$encode
        }
        if(!is_empty(hdrs))
            req$headers <- hdrs
        if(!is_empty(self$body))
            req$body <- self$body
        if(!is_empty(self$content_type))
            req$content_type <- self$content_type
        req
    }
))


#' Call the Graph API batch endpoint
#'
#' @param token An Azure OAuth token, of class [AzureToken].
#' @param requests A list of [graph_request] objects, representing individual requests to the Graph API.
#' @param depends_on An optional named list indicating which requests have dependencies. See the examples below.
#' @param api_version The API version to use, which will form part of the URL sent to the host.
#'
#' @details
#' Use this function to combine multiple requests into a single HTTPS call. This can save significant network latency.
#'
#' @seealso
#' [graph_request], [call_graph_endpoint]
#'
#' [Microsoft Graph overview](https://docs.microsoft.com/en-us/graph/overview),
#' [Batch endpoint documentation](https://docs.microsoft.com/en-us/graph/json-batching)
#'
#' @examples
#' \dontrun{
#'
#' req1 <- graph_request$new("me")
#'
#' # a new email message in Outlook
#' req2 <- graph_request$new("me/messages",
#'     body=list(
#'         body=list(
#'             content="Hello from R",
#'             content_type="text"
#'         ),
#'         subject="Hello",
#'         toRecipients="bob@example.com"
#'     ),
#'     http_verb="POST"
#' )
#'
#' # messages in drafts
#' req3 <- graph_request$new("me/mailFolders/drafts/messages")
#'
#' # depends_on signifies that request 3 depends on request 2
#' call_batch_endpoint(token, list(req1, req2, req3), depends_on=list("3"=2))
#'
#' }
#' @export
call_batch_endpoint <- function(token, requests=list(), depends_on=list(),
                                api_version=getOption("azure_graph_api_version"))
{
    for(req in requests)
        if(!inherits(req, "graph_request"))
            stop("Must supply a list of request objects", call.=FALSE)

    if(length(requests) > 20)
        stop("Maximum of 20 requests per batch job", call.=FALSE)

    ids <- as.character(seq_along(requests))
    id_depends <- names(depends_on)
    if(!is_empty(id_depends) && !all(id_depends %in% ids))
        stop("'depends_on' should be a named list identifying dependencies")

    # populate the batch request body
    reqlst <- lapply(requests, function(req) req$batchify())
    for(i in seq_along(reqlst))
        reqlst[[i]]$id <- as.character(i)

    # insert depends_on if required
    if(!is_empty(depends_on))
    {
        if(is_empty(names(depends_on)))
            names(depends_on) <- seq_along(requests)
        for(i in seq_along(depends_on))
        {
            id <- as.numeric(names(depends_on)[i])
            reqlst[[id]]$depends_on <- unname(depends_on[i])
        }
    }

    reslst <- call_graph_endpoint(token, "$batch", body=list(requests=reqlst),
        http_verb="POST", api_version=api_version)$responses

    reslst <- reslst[order(sapply(reslst, `[[`, "id"))]
    err_msgs <- lapply(reslst, function(res)
    {
        if(res$status >= 300)
            error_message(res$body)
        else NULL
    })
    errs <- !sapply(err_msgs, is.null)
    if(any(errs))
        stop("Graph batch job encountered errors on requests ", paste(err_msgs[errs], collapse=", "),
             "\nMessages:\n",
             paste(unlist(errs), collapse="\n"),
             call.=FALSE)
    reslst
}

