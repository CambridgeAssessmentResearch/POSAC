#' Update POSAC output objects given new X and Y (perhaps from movecheck or swapcheck).
#'
#' This function takes the output from the function POSAC along with new vectors of X and Y co-ordinates and updates the object to reflect the change.
#' @param posacout Output from function POSAC (or something with similar parts).
#' @param newX Vector of new values for the X co-ordinates (will be converted to ranks).
#' @param newY Vector of new values for the Y co-ordinates (will be converted to ranks).
#'
#' @return The function returns a list with the following elements:
#' \describe{
#'   \item{Criteria}{The output from the function realcriteria for the new solution.}
#'   \item{X}{The ranking of each pattern on the first dimension of the returned POSAC solution.}
#'   \item{Y}{The ranking of each pattern on the second dimension of the returned POSAC solution.}
#'   \item{Patterns}{A character vector of concatenated patterns.}
#'   \item{patmat}{The pattern matrix used in analysis to begin with.}
#'   \item{freqs}{The frequency with which in pattern occurs.}
#'   \item{tidyframe}{A single data frame containing the structuples(patterns), frequencies, X, Y, rescaled X and Y to be between a 0 and 100, J (=rescaled X+rescaled Y)and L(=100+rescaled X-rescaled Y).}
#'   \item{mumatrix}{Matrix of monotonicity coefficients between each of the original variables and the resulting rescaled X, Y, J and L.}
#' }
#'
#' @keywords Scalogram
#' @export
#' @examples
#' #see help for movecheck function

POSACupdate=function(posacout,newX,newY){

patmat=posacout$patmat
npat=nrow(patmat)
freqs=posacout$freqs
gam<-gamfunc(patmat)
X<-rank(newX)
Y<-rank(newY)
#break any ties (say incomparable)
rand1=runif(npat,-0.01,0.01)
X=X+rand1
Y=Y-rand1
X<-rank(X)
Y<-rank(Y)

crits=realcriteria(c(X,Y),gam,Freq=freqs)

#TIDY DATA FRAME
pattern<-apply(patmat,1,paste,collapse="")
rescX=round(100*(X-min(X))/(max(X)-min(X)))
rescY=round(100*(Y-min(Y))/(max(Y)-min(Y)))
tidyframe=data.frame(Structuple=pattern,freqs=freqs,X=X,Y=Y,rescX=rescX,rescY=rescY,J=rescX+rescY,L=100+rescX-rescY)

#mu correlation matrix between original patterns and discovered dimensions
mumatrix=matrix(mapply(function(i,j) tryCatch(mu(patmat[,i],tidyframe[,j]),error=function(e) NA)
	,rep(1:ncol(patmat),4)
	,rep(5:8,each=ncol(patmat))),nrow=ncol(patmat))
rownames(mumatrix)=colnames(patmat)
colnames(mumatrix)=names(tidyframe)[5:8]
mumatrix

return(list(Criteria = crits, X = X, Y = Y, Patterns = pattern
		,patmat = patmat, freqs = freqs
		,tidyframe = tidyframe,mumatrix=mumatrix))

}


