#'
#' Write Data Frame 
#' 
#' The function writes data frame with its colnames to a file, using '\t' as filed separator. 
#' 
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.
#' @param fn a character string naming a file 
#' 
#' @note write.data.frame() does not return rownames of x. No columns will be surrounded by double quotes.
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
#' write.matrix()
#'
#' 
#' @examples 
#'
#' @export
#' 


write.data.frame <-
function (x, fn) 
write.table(x, fn, col.names = TRUE, row.names = FALSE, quote = FALSE, 
    sep = "\t")
