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
