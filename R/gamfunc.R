#' Function to create the gamma matrix
#'
#' This function calculates the gamma matrix. That is, the matrix indicating whether each unique pattern is superior (+1), inferior (-1) or
#' incomparable (0) to each other unique pattern.
#'
#' @param CR A matrix of ratings. Rows should represent observations and columns should represent variables.
#' @export
#' @examples
#' gamfunc(SimplePOSACdata[,1:4])

gamfunc<-function(CR){
	temp1<-matrix(rep(0,length(CR[,1])^2),ncol=length(CR[,1]))
	temp2<-matrix(rep(0,length(CR[,1])^2),ncol=length(CR[,1]))
	for (iz in 1:length(CR[1,])){
	Ciz<-CR[,iz]
	Cizmat<-Ciz%*%t(rep(1,length(Ciz)))
	temp1<-temp1+(Cizmat>t(Cizmat))
	temp2<-temp2+(Cizmat<t(Cizmat))
	}
	temp1<-(temp1>0)+0
	temp2<-(temp2>0)+0
	gam<-temp1*(1-temp2)-temp2*(1-temp1)
	return(gam)}
