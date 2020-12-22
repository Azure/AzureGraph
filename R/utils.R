#' Miscellaneous utility functions
#'
#' @param lst A named list of objects.
#' @param name_fields The components of the objects in `lst`, to be used as names.
#' @param x For `is_empty`, An R object.
#' @details
#' `named_list` extracts from each object in `lst`, the components named by `name_fields`. It then constructs names for `lst` from these components, separated by a `"/"`.
#'
#' @return
#' For `named_list`, the list that was passed in but with names. An empty input results in a _named list_ output: a list of length 0, with a `names` attribute.
#'
#' For `is_empty`, whether the length of the object is zero (this includes the special case of `NULL`).
#'
#' @rdname utils
#' @export
named_list <- function(lst=NULL, name_fields="name")
{
    if(is_empty(lst))
        return(structure(list(), names=character(0)))

    lst_names <- sapply(name_fields, function(n) sapply(lst, `[[`, n))
    if(length(name_fields) > 1)
    {
        dim(lst_names) <- c(length(lst_names) / length(name_fields), length(name_fields))
        lst_names <- apply(lst_names, 1, function(nn) paste(nn, collapse="/"))
    }
    names(lst) <- lst_names
    dups <- duplicated(tolower(names(lst)))
    if(any(dups))
    {
        duped_names <- names(lst)[dups]
        warning("Some names are duplicated: ", paste(unique(duped_names), collapse=" "), call.=FALSE)
    }
    lst
}


#' @rdname utils
#' @export
is_empty <- function(x)
{
    length(x) == 0
}
