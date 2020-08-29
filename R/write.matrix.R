#'
#' Write Matrix 
#' 
#' The function writes data frame without colnames to a file, using '\t' as filed separator. 
#' 
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.
#' @param fn a character string naming a file 
#' 
#' @note write.matrix() does not return rownames or colnames of x. No columns will be surrounded by double quotes.
#'
#' 
#' @return prints x to a file named by fn
#'
#' 
#' @author Xia Shen
#' 
#' @references 
#' 
#' @seealso 
#' write.data.frame()
#'
#' 
#' @examples 
#'
#' @export
#' 



write.matrix <-
function (x, fn) 
write.table(x, fn, col.names = FALSE, row.names = FALSE, quote = FALSE, 
    sep = "\t")
