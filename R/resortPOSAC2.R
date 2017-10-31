#' Further optimisation via sorting.
#' 
#' This function is intended to ensure broad directions are corrected (i.e. most dominant in top right and least dominant in bottom left) and then
#' performs sorting to improve the target criteria.
#' This function is not intended for use directly but rather is used as a subroutine of the main POSAC function.
#'
#' @param XY A concatenated vector of the the x-coordinates and y-coordinates for each combination of values for the covariates (i.e. should have length 2n if n if the number of combinations we are interested in).
#' @param gam2 The matrix of squared gamma values (gam2)
#' @param CR The matrix of ratings.
#' @export

#FUNCTION TO DO BROAD DIRECTION CHANGES AND THEN SORT INDIVIDUAL CASES OUT
resortPOSAC2<-function(XY,gam,CR){
nv<-length(XY)/2
X<-XY[1:nv]
Y<-XY[(nv+1):(2*nv)]
D<-data.frame(X=rank(X),Y=rank(Y),J=X+Y,gsum=rowSums(gam),S=rowSums(CR),ord=seq(1,length(X)))
#split D into extreme and non-extreme
Dext<-D[abs(D$gsum)==(length(D[,1])-1),]
Dnoext<-D[abs(D$gsum)<(length(D[,1])-1),]
#if we have extremes (and we can have at most 2)
#then check they are max and min and reverse if necessary
if (dim(Dext)[1]==2){
	Dext<-Dext[order(Dext$S),]	
	Dext$X<-c(min(D$X)-1,max(D$X)+1)
	Dext$Y<-c(min(D$Y)-1,max(D$Y)+1)
	Dext$J<-Dext$X+Dext$Y
	#print(Dext)
	}
if (dim(Dext)[1]==1){
	if (Dext$gsum<0){Dext$X<-c(min(D$X)-1)
			Dext$Y<-c(min(D$Y)-1)
			Dext$J<-Dext$X+Dext$Y}
	if (Dext$gsum>0){Dext$X<-c(max(D$X)+1)
			Dext$Y<-c(max(D$Y)+1)
			Dext$J<-Dext$X+Dext$Y}
	}
#check correlation between J and S for non-extreme and reverse if necessary
if (cor(Dnoext$J,Dnoext$S)<0){
	Dnoext$X<-(1+max(D$X)-Dnoext$X)
	Dnoext$Y<-(1+max(D$Y)-Dnoext$Y)
	Dnoext$J<-Dnoext$X+Dnoext$Y}
D2<-Dnoext
if (dim(Dext)[1]>0){D2<-rbind(Dext,Dnoext)}
#put back in oridinal order
D2<-D2[order(D2$ord),]
#now use resortPOSAC to deal with any remaining difficulties
XY2<-c(D2$X,D2$Y)
return(resortPOSAC(XY2,gam))}
