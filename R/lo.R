#'
#' Summarize all objects in the workspace
#' 
#' The function summarizes all the objects in the workspace in a human readable format. 
#' 
#' @param pos See ?ls pos argument. Default = 1 for the used input objects.
#' @param pattern See ?ls pattern argument. List objects with specified pattern.
#' @param order.by Ordering by which column. 1: Class; 2 (Default): Size; 3: Row number; 4: Column number.
#' @param decreasing Whether to list in decreasing order. Default: \code{TRUE}.
#' @param head Whether to head the list. Default: \code{TRUE}.
#' @param n Number of objects to be considered in \code{head}.
#'
#' @note Normally no argument need to be specified.
#'
#' 
#' @return A summary list will be printed on the screen.
#'
#' 
#' @author Xia Shen
#' 
#' @references 
#' None
#' 
#' @examples 
#'\dontrun{
#'
#' lo()
#'
#' }
#' @export
#' 

lo <-
function (pos = 1, pattern, order.by = 2, decreasing = TRUE, 
    head = TRUE, n = 99) 
{
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, 
        pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- round(napply(names, object.size)/1e+06, digits = 2)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Class", "Size (MB)", "Rows", "Columns")
    out <- out[order(out[[order.by]], decreasing = decreasing), 
        ]
    if (head) 
        out <- head(out, n)
    out
}
