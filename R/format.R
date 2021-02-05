#' Format a Microsoft Graph or Azure object
#'
#' Miscellaneous functions for printing Microsoft Graph and Azure R6 objects
#'
#' @param env An R6 object's environment for printing.
#' @param exclude Objects in `env` to exclude from the printout.
#'
#' @details
#' These are utilities to aid in printing R6 objects created by this package or its descendants. They are not meant to be called by the user.
#'
#' @rdname format
#' @export
format_public_fields <- function(env, exclude=character(0))
{
    objnames <- ls(env)
    std_fields <- c("token", "extra")
    objnames <- setdiff(objnames, c(exclude, std_fields))

    maxwidth <- as.integer(0.8 * getOption("width"))

    objconts <- sapply(objnames, function(n)
    {
        x <- get(n, env)
        deparsed <- if(is_empty(x) || is.function(x)) # don't print empty fields
            return(NULL)
        else if(is.list(x))
            paste0("list(", paste(names(x), collapse=", "), ")")
        else if(is.vector(x))
        {
            x <- paste0(x, collapse=", ")
            if(nchar(x) > maxwidth - nchar(n) - 10)
                x <- paste0(substr(x, 1, maxwidth - nchar(n) - 10), " ...")
            x
        }
        else deparse(x)[[1]]

        paste0(strwrap(paste0(n, ": ", deparsed), width=maxwidth, indent=2, exdent=4),
               collapse="\n")
    }, simplify=FALSE)

    empty <- sapply(objconts, is.null)
    objconts <- objconts[!empty]

    # print etag at the bottom, not the top
    if("etag" %in% names(objconts))
        objconts <- c(objconts[-which(names(objconts) == "etag")], objconts["etag"])

    paste0(paste0(objconts, collapse="\n"), "\n---\n")
}


#' @rdname format
#' @export
format_public_methods <- function(env)
{
    objnames <- ls(env)
    std_methods <- c("clone", "print", "initialize")
    objnames <- setdiff(objnames, std_methods)
    is_method <- sapply(objnames, function(obj) is.function(.subset2(env, obj)))

    maxwidth <- as.integer(0.8 * getOption("width"))

    objnames <- strwrap(paste(objnames[is_method], collapse=", "), width=maxwidth, indent=4, exdent=4)
    paste0("  Methods:\n", paste0(objnames, collapse="\n"), "\n")
}
