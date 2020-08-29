#'
#' Find text in all the files in a directory
#' 
#' The function finds a specific pattern of text in a specific directory. 
#' 
#' @param x A specific pattern to be found.
#' @param loc A string giving the directory to search. Defaults to the current directory.
#'
#' @note None.
#'
#' 
#' @return A series of grep results will be printed on the screen.
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
#' grepinfile('a')
#'
#' }
#' @export
#' 

grepinfile <-
function (x, loc = "./") 
{
    files <- list.files(loc)
    for (i in 1:length(files)) {
        cat(i, files[i], "\n")
        system(paste("grep ", x, " ", files[i], sep = ""))
    }
}
