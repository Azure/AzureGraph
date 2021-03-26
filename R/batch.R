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

    body <- list(requests=reqlst)
    call_graph_endpoint(token, "$batch", body=body, http_verb="POST", api_version=api_version)
}

