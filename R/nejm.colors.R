#'
#' nejm style colors 
#' 
#' The function generates colors character for nejm likely
#' 
#' @param n An integer for the number of colors,  default 8,  required <=8.
#' @param  alpha An numeric for alpha, default 1, requied >=0 and <=1.
#' @note NULL
#'
#' 
#' @return NULL
#' 
#' @author Xia Shen
#' 
#' @references 
#' NULL
#' 
#' @seealso 
#' NULL
#'
#' @examples 
#'\dontrun{
#'
#' nejm.colors(7,0.2)
#'
#' }
#' @export
#' 

nejm.colors <-
function (n = 8, alpha = 1) 
{
    pal <- c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", 
        "#6F99AD", "#FFDC91", "#EE4C97")
    acode <- substr(rgb(0, 0, 0, alpha = alpha), 8, 9)
    return(paste0(pal, acode)[1:n])
}
