#' Differentiable function approximating the U matrix (see realU).
#'
#' This function is not intended for use directly but rather is used as part of the main POSAC function.
#'
#' @param XY A concatenated vector of the the x-coordinates and y-coordinates for each combination of values for the covariates (i.e. should have length 2n if n if the number of combinations we are interested in).
#' @param gam2. The matrix of squared gamma values (gam2)
#' @param Freq. The number of observations with each combination of covariate values.
#' @export

Ufunc<-function(XY,gam2,Freq){
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
	#limit Umiddle to 700 to avoid infinities
	Umiddle<-Umiddle*(Umiddle<=700)+700*(Umiddle>700)
	Umat<-(exp(Umiddle)/(1+exp(Umiddle)))
	U<-sum(FiFj*Umat)
	return(U)}