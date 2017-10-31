#' Function to swap initial results so that greater and lesser are distringuished.
#'
#' Sorts starting with lowest values of J and working to highest. This function is not intended for use directly but rather is used as a subroutine of the main POSAC function.
#'
#' @param XY A concatenated vector of the the x-coordinates and y-coordinates for each combination of values for the covariates (i.e. should have length 2n if n if the number of combinations we are interested in).
#' @param gam2 The matrix of squared gamma values (gam2)
#' @export

resortPOSAC<-function(XY,gam){
nv<-length(XY)/2
X<-XY[1:nv]
Y<-XY[(nv+1):(2*nv)]

D<-data.frame(X,Y,ord=seq(1,length(X)))
#sort D by J before starting
D<-D[order(X+Y),]
gam<-gam[order(X+Y),order(X+Y)]

for (iz in 1:(nv-1)){
for (jz in (iz+1):nv){
	curx1<-D$X[iz]
	cury1<-D$Y[iz]
	curx2<-D$X[jz]
	cury2<-D$Y[jz]
	if (gam[iz,jz]==1 & D$X[iz]<=D$X[jz] & D$Y[iz]<=D$Y[jz]){
		D$X[iz]<-curx2
		D$Y[iz]<-cury2
		D$X[jz]<-curx1
		D$Y[jz]<-cury1}}}

#put back into original order
D<-D[order(D$ord),]

return(c(D$X,D$Y))
}

