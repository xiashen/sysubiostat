
#'
#' Generate colors like ggplot style 
#' 
#' This function generates a number of ggplot style colors  
#' 
#' @param n An integer for the number of colors.
#'
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
#' ggcolor(8)
#'
#' }
#' @export
#' 

ggcolor<- function(n){
    ggcolor2 <- scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1)
    ggcolor2(n)
}