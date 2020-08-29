#'
#' 
#' 
#' The function conducts GWAS or multiple linear regression
#' 
#' @param y A vector of the geneotype of each individual.
#' @param X A matrix or data.frame of several phenotypes or one phenotypes of each individual,requring X must be an array of at least two dimensions

#' @note you can also use this fuction to conduct linear regression.
#'
#' 
#' @return A data.fame of beta, se, p-vaule
#'
#' 
#' @author Ting Li, Xia Shen
#' 
#' @references 
#' NULL
#' 
#' @seealso 
#' NULL
#'
#' 
#' @examples 
#'
#' a <- sample(c(0,1,2),10,replace=T)	
#' b <- matrix(rnorm(50,2,1),10,5)
#' gwas(a,b)
#' 
#' @export
#' 

gwas <- function (y, X) 
{
    n <- length(y)
    U1 = sum(y)
    U2 = U1/n
    ytr = y - rep(1, n) * U2
    U3 = colSums(X)
    U4 = U3/n
    Str = X - tcrossprod(rep(1, n), U4)
    Str2 = colSums(Str^2)
    b = as.vector(crossprod(ytr, Str)/Str2)
    sig = (sum(ytr^2) - b^2 * Str2)/(n - 2)
    err = sqrt(sig * (1/Str2))
    pval = pchisq(b^2/err^2, 1, low = FALSE)
    return(data.frame(beta = b, se = err, p = pval))
}




