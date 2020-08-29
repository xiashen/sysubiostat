#'
#' The function calculates the variance of each column of your data table
#' 
#' 
#' @param X A data.frame or matrix you want to calculate the variance of each column 

#' @note NULL
#'
#' 
#' @return A vecotor of variance of each column. 
#'
#' 
#' @author Ting Li, Xia Shen
#' 
#' @references 
#' NULL
#' 
#' @seealso 
#' \code{colSums}
#' \code{colMeans}
#' 
#' @examples 
#'
#' df <- matrix(rnorm(10,0,1),5,2)
#' df
#' res <- colVars(df)
#' 
#' @export
#' 

colVars <- function (x) colSums((t(t(x) - colMeans(x)))^2)/(nrow(x) - 1)




