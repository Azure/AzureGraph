ms_graph_pager <- R6::R6Class("ms_graph_pager",

public=list(

    token=NULL,
    value_name=NULL,
    next_link_name=NULL,
    next_link=NULL,
    output=NULL,
    type_filter=NULL,
    init_args=NULL,

    initialize=function(token, first_page, next_link_name="@odata.nextLink", value_name="value",
                        output=c("list", "data.frame", "object"), type_filter=NULL, ...)
    {
        self$token <- token
        self$value_name <- value_name
        self$next_link_name <- next_link_name
        self$next_link <- first_page[[next_link_name]]
        private$first_value <- first_page[[value_name]]
        self$output <- match.arg(output)
        self$type_filter <- type_filter
        self$init_args <- list(...)
    }
),

active=list(

    value=function()
    {
        if(!is.null(private$first_value))
        {
            first_value <- private$first_value
            private$first_value <- NULL
            return(private$make_objects(first_value))
        }
        if(is.null(self$next_link))
            return(NULL)

        lst <- call_graph_url(self$token, self$next_link, simplify=(self$output == "data.frame"))
        self$next_link <- lst[[self$next_link_name]]
        private$make_objects(lst[[self$value_name]])
    }
),

private=list(

    first_value=NULL,

    make_objects=function(lst)
    {
        if(self$output != "object")
            return(lst)

        lst <- lapply(lst, function(obj)
        {
            class_gen <- find_class_generator(obj, self$type_filter)
            if(is.null(class_gen))
                NULL
            else do.call(class_gen$new, c(list(self$token, self$tenant, obj), self$init_args))
        })
        lst[!sapply(lst, is.null)]
    }
))
