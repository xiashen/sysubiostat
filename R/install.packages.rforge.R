#'
#' Install package from R-Forge
#' 
#' The function installs packages from the R-Forge. 
#' 
#' @param x A string giving the package name.
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
#' install.packages.rforge('sysubiostat')
#'
#' }
#' @export
#' 

install.packages.rforge <-
function (x) 
install.packages(x, repos = "http://r-forge.r-project.org")
