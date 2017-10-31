#' Function to calculate Guttman's monotonicity coefficient (mu) between two vectors
#'
#' The coefficient expresses the extent to which values on one variable increase in a particular direction along with the values on
#' another variable without assuming that the increase is exactly according to a straight line.
#'
#' @param x First vector
#' @param y Second vector
#' @return a number between -1 and +1
#' @examples
#' x1=rnorm(1000)
#' x2=x1^3+rnorm(1000)
#' #Compare three coefficients of association and plot association
#' cor(x1,x2)
#' cor(x1,x2,method="spearman")
#' mu(x1,x2)
#' plot(x1,x2)
#' @export

mu <- function(x,y) {
  if (length(x)!=length(y)){
    stop ("Both vectors must be the same length.")
  }
  xm<-matrix(x,nrow=length(x),ncol=length(x),byrow=FALSE)
  ym<-matrix(y,nrow=length(y),ncol=length(y),byrow=FALSE)
  num<-sum(rowSums((xm-t(xm))*(ym-t(ym))))
  denom<-sum(rowSums(abs(xm-t(xm))*abs(ym-t(ym))))
  if (denom==0){
    stop ("Denominator is zero.  Monotonicity coefficient cannot be calculated.")
    return(NA)
  } else {
    return (num/denom)
  }
}

