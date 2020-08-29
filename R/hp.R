#'
#' Drawing heatmap
#' 
#' This function is an user-friendly and automatic heatmap drawing function. 
#' 
#' @param x a matrix of values to be ploted.
#' @param output.path a character of the output pdf file path. Default is '~/Desktop/heatmap.pdf'.
#' @param mode It indicates the mode of heatmap. It should be 'matrix' or 'melt'. If mode = 'matrix', please input your raw matrix. If you want to input data in melt form after melt() function of reshape2 package.
#' @param x.lab a character of x axis label. Default is NA.
#' @param y.lab a character of y axis label. Default is NA.
#' @param pmatrix a matrix of p-values, not necessary.
#' @param legend a character of legend name. Default is '-log10(P)'.
#' @param low.color a integer indicates the color you want to use when value is low. Default is 10.
#' @param high.color a integer indicates the color you want to use when value is high. Default is 1.
#' @param mid.color a character indicates the color you want to use for mid-value. Default is 'white'.
#' @param title a character of figure title. Default is NA.
#' @param basepoint a integer of your mid-value. Default is 0.
#' @param pdf.width  a integer of width of the output pdf. Default is NA for automatic setting.
#' @param pdf.height a integer of height of the output pdf. Default is NA for automatic setting.
#' @param axis.lab a logical value decides whether label of axis to be shown. Default is FALSE.
#' @param legend.break a vector of break points for legend. Default is NA.
#' @param sig.level significant levels of pmatrix(p-values). Default is 0.05.
#' @param sig.lab label of significant results. Default is '*'.
#'
#' @note Please change the parameter \code{output.path} to makesure the output pdf file will be produced in your target directory. The parameters only need to be set is \code{x} the matrix you want to draw and \code{output.path} for a very easy using.
#' If you want to draw something like enrichment values, and highlight the significant results, you can input the enrichment values matrix as \code{x} and input the p-values matrix as \code{pmatrix}. And also you can set significant levels of p-values as \code{sig.level}.
#'
#' 
#' @return A pdf file for heatmap produced in \code{output.path}.
#'
#' 
#' @author Zhijian Yang
#' 
#' @references 
#' None
#' 
#' @seealso 
#' None
#' 
#' @examples 
#' \dontrun{
#' 
#' set.seed(666)
#' a <- matrix(rnorm(30,sd = 3),ncol = 5)
#' hp(a)
#' # null device 
#' #          1 
#' 
#' p <- matrix(runif(30),ncol = 5)
#' hp(a,output.path = "~/Downloads//heatmap.pdf",pmatrix = p)
#' # null device 
#' #          1 
#' # Warning message:
#' # Removed 29 rows containing missing values (geom_text). 
#'}
#' 
#' @export
#' @import ggplot2, RColorBrewer, reshape2

hp <- function(x,output.path = "~/Desktop/heatmap.pdf",mode = "matrix",x.lab = NA,y.lab = NA,pmatrix = NA,legend = "-log10(P)",
              low.color = 10,high.color = 1,mid.color="white",title = NA,basepoint = 0,pdf.width = NA,pdf.height = NA,
              axis.lab = F,legend.break = NA,sig.level = 0.05,sig.lab = "*") {

  require(ggplot2)
  require(RColorBrewer)
  require(reshape2)
  
  if (mode == "matrix") {
    x <- melt(x)
  } else if(mode == "melt") {
    x.lab <- colnames(x)[1]
    y.lab <- colnames(x)[2]
    colnames(x) <- c("Var1","Var2","value")
  }
  
  if(is.na(pdf.height)) {
    pdf.height <- length(unique(x$Var2))/2.8
  }
  if (is.na(pdf.width)) {
    pdf.width <- length(unique(x$Var1))/2.5
  }
  
  if (pdf.height < 4) {pdf.height = 3.5}
  if (pdf.width < 4) {pdf.width = 4}
  
  x$Var1 <- as.character(x$Var1)
  x$Var2 <- as.character(x$Var2)
  
  # print(head(x))
  # print(str(x))
  
  if(high.color %in% c(1:10) & low.color %in% c(1:10)) {
    color = brewer.pal(10, "Spectral")
    lowcolor = color[low.color]
    highcolor = color[high.color]
  } else if (!highcolor %in% c(1:10)) {
    highcolor = high.color
  } else if (!lowcolor %in% c(1:10)) {
    lowcolor = low.color
  }

  
  hp <- ggplot(x,aes(x = Var1,y = Var2,fill = value)) + geom_tile() 
  hp <- hp + theme_classic() + theme(axis.ticks = element_blank(),axis.line = element_blank())
  hp <- hp + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  hp <- hp + theme(legend.title = element_text(face = "bold",size = unit(pdf.width*1.4,"mm")),legend.text = element_text(size = unit(pdf.width*1.5,"mm")))
  if (all(na.omit(x$value >= basepoint))) {
    hp <- hp + scale_fill_gradient2(legend,low = mid.color,high = highcolor)
  } else if (all(na.omit(x$value < basepoint))) {
  	hp <- hp + scale_fill_gradient2(legend,low = lowcolor,high = mid.color)
  } else if (any(!is.na(legend.break))) {
    hp <- hp + scale_fill_gradient2(breaks = legend.break,low = lowcolor,mid=mid.color,midpoint = basepoint,high = highcolor,guide = guide_colorbar(title = legend,barheight = unit(pdf.height*5,"mm"),barwidth = unit(pdf.width*1.2,"mm")))
  } else {
    hp <- hp + scale_fill_gradient2(low = lowcolor,mid=mid.color,midpoint = basepoint,high = highcolor,guide = guide_colorbar(title = legend,barheight = unit(pdf.height*5,"mm"),barwidth = unit(pdf.width*1.2,"mm")))
  }
  
  if (is.na(x.lab) & is.na(y.lab)){
    hp <- hp + theme(axis.title = element_blank())
  } else {
    hp <- hp + xlab(label = x.lab) + ylab(lael = y.lab)
  }
  
  if (!axis.lab) {
    hp <- hp + theme(axis.text = element_blank())
  }
  
  if(!is.na(title)) {
    hp <- hp + labs(title = title) + theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  }
  
  if (all(!is.na(pmatrix))) {
    pmelt <- melt(as.matrix(pmatrix))
    pmelt$star <- NA
    pmelt$star[pmelt$value < sig.level] <- sig.lab
    hp <- hp + geom_text(aes(label=pmelt$star),col ="black",size = 5,nudge_y = -0.12)
  }
  
  pdf(output.path,width = pdf.width,height = pdf.height)
  print(hp)
  dev.off()
}

