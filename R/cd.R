#'
#' Change working directory using the cd syntax
#' 
#' The function enables changing working directory using the cd syntax. 
#' 
#' @param x A string giving the directory relative to the current working directory.
#'
#' @note None.
#'
#' 
#' @return None.
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
#' system('mkdir test')
#' 
#' cd('test')
#'
#' }
#' @export
#' 

cd <-
function (x = NULL) 
if (is.null(x)) setwd("~") else setwd(paste(getwd(), "/", x, 
    sep = ""))
