#'
#' Compile an Rmarkdown source document
#' 
#' The function compiles Rmarkdown Rnw file to PDF document. 
#' 
#' @param fn Rnw file name with path.
#'
#' @note None.
#'
#' 
#' @return A PDF document will be created in the specified directory.
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
#' }
#' @export
#' 

compileRnw <-
function (fn) 
{
    Sweave2knitr(paste(fn, ".Rnw", sep = ""))
    knit2pdf(paste(fn, "-knitr.Rnw", sep = ""))
}
