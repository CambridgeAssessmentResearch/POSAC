#' Function to calculate the U matrix.
#'
#' This function is not intended for use directly but rather is used as a subroutine of the main POSAC function.
#' It counts the (weighted) number of pairs that are correctly positioned as either comparable or non-comparable.
#' @param XY A concatenated vector of the the x-coordinates and y-coordinates for each combination of values for the covariates (i.e. should have length 2n if n if the number of combinations we are interested in).
#' @param gam2 The matrix of squared gamma values (gam2)
#' @param Freq The number of observations with each combination of covariate values.
#' @export

realU<-function(XY,gam2,Freq){
	#Create FiFj matrix
	FiFj<-Freq%*%t(Freq)
	#Create X and Y
	nv<-length(XY)/2
	X<-XY[1:nv]
	Y<-XY[(nv+1):(2*nv)]
	#Create Xi-Xj matrix and Yi-Yj matrix
	XimXj<-(X%*%t(rep(1,length(X))))-t(X%*%t(rep(1,length(X))))
	YimYj<-(Y%*%t(rep(1,length(Y))))-t(Y%*%t(rep(1,length(Y))))
	#Now create the U matrix (same as p matrix)
	Umiddle<-3*(2*gam2-1)*(XimXj)*(YimYj)
	#ONLY CHANGE (FROM UFUNC) IS HERE
	Umat<-(Umiddle>0)+0
	#print(diag(Umat))
	#Divide by 2 so we don't double count
	U<-sum(FiFj*Umat)/2
	return(U)}
