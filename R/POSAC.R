#' Partial Order Scalogram Analysis with Base Coordinates.
#'
#' This functions takes a matrix of patterns and the frequencies of the patterns and performs POSAC analysis.
#'
#' @param patmat A matrix of patterns of values across. Each row of the matrix should represents a distinct pattern with no duplicates. The columns are the variables defining the patterns.
#' @param freqs A vector of frequencies of each pattern. This should have the same length as nrow(patmat). 
#' @param greedy Logical value denoting whether optimisation method should finish with a greedy search through all possible movements of each point in order to improve the number of pairs of patterns that are correctly positioned. Setting this to true will make the process somewhat slower. The default value is FALSE. 
#'
#' @return The function returns a list with the following elements:
#' \describe{
#'   \item{Criteria}{The output from the function realcriteria for the final solution.}
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
#' #POSAC analysis of some simple patterns
#' posac1=POSAC(SimplePOSACdata[,1:4],SimplePOSACdata[,5])
#' posac1
#' plot(posac1$X,posac1$Y,type='n',xlab="Facet analysis X",ylab="Facet analysis Y")
#' text(posac1$X,posac1$Y,posac1$Patterns)
#'
#' #POSAC analysis of some CRAS patterns
#' posac2=POSAC(CRASdata[,1:5],CRASdata[,6])
#' posac2
#' plot(posac2$X,posac2$Y,type='n',xlab="Facet analysis X",ylab="Facet analysis Y")
#' text(posac2$X,posac2$Y,posac2$Patterns)
#' posac2$tidyframe
#'
#' #An enhanced plot with colour used to indicate the frequency of patterns
#' library(ggplot2)
#' g1=ggplot(data=data.frame(X=posac2$X
#' 				,Y=posac2$Y
#' 				,pat=posac2$Patterns
#' 				,Freq=posac2$freqs),
#' 	aes(x=X,y=Y,label=pat,col=Freq))
#' g1+geom_text()+theme_minimal()+labs(x="Facet analysis X",y="Facet analysis Y")
#'
#' #make an item diagram using ggplot2 (can also be done using the itemdiagram function)
#' g2=ggplot(data=data.frame(X=posac2$X,Y=posac2$Y),
#'           aes(x=X,y=Y,label=posac2$patmat$complex,col=as.factor(posac2$patmat$complex)))
#' g2+geom_text()+geom_point()+theme_minimal()+labs(x="Facet analysis X",y="Facet analysis Y")+guides(col="none")

POSAC=function(patmat,freqs,greedy=FALSE){

pattern <- apply(patmat, 1, paste, collapse = "")
npat = dim(patmat)[1]
nvar = dim(patmat)[2]
gam <- gamfunc(patmat)

#sets of starting values
startvals=list()
#starting values 1 (all incomparable just based on order in the data)
startvals[[1]]=list()
startvals[[1]]$X=(1/npat)*rank(rep(0,npat),ties.method="first")
startvals[[1]]$Y=(1/npat)*rank(rep(0,npat),ties.method="last")
#starting values 2 (all comparable based on sum of all columns)
startvals[[2]]=list()
startvals[[2]]$X=(1/npat)*rank(rowSums(as.matrix(patmat)),ties.method="first")
startvals[[2]]$Y=(1/npat)*rank(rowSums(as.matrix(patmat)),ties.method="last")
#starting values 3 (add up first and second halves)
startvals[[3]]=list()
startvals[[3]]$X=(1/npat)*rank(rowSums(as.matrix(patmat[,1:(floor(nvar)/2)])),ties.method="first")
startvals[[3]]$Y=(1/npat)*rank(rowSums(as.matrix(patmat[,-(1:(floor(nvar)/2))])),ties.method="last")
#starting values 4 (add up odd and even halves)
startvals[[4]]=list()
startvals[[4]]$X=(1/npat)*rank(rowSums(as.matrix(patmat[,((1:nvar)%%2)==1])),ties.method="first")
startvals[[4]]$Y=(1/npat)*rank(rowSums(as.matrix(patmat[,((1:nvar)%%2)==0])),ties.method="last")

bestX=startvals[[1]]$X
bestY=startvals[[1]]$Y
bestcrit=realcriteria(c(bestX, bestY), gam, Freq = freqs)$overall$pccorrect

###function for differentiable version of realcriteria will base on tanh of differences
dtanh=function(x){1-(tanh(x)^2)}
CritDif=function(XY,gam,Freq){
#XY=c(X,Y)
#Freq=freqs
npat=length(XY)/2
X=XY[1:npat]
Y=XY[-(1:npat)]
XmX=matrix(X,ncol=1)%*%matrix(rep(1,npat),nrow=1)-matrix(rep(1,npat),ncol=1)%*%matrix(X,nrow=1)
YmY=matrix(Y,ncol=1)%*%matrix(rep(1,npat),nrow=1)-matrix(rep(1,npat),ncol=1)%*%matrix(Y,nrow=1)
gamest=(tanh(3*XmX)+tanh(3*YmY))/2
FiFj <- Freq %*% t(Freq)
sum(FiFj*((gamest-gam)^2))
}

#gradient function (subroutine)
grCritDif=function(XY,gam,Freq){
  npat=length(XY)/2
  X=XY[1:npat]
  Y=XY[-(1:npat)]
  XmX=matrix(X,ncol=1)%*%matrix(rep(1,npat),nrow=1)-matrix(rep(1,npat),ncol=1)%*%matrix(X,nrow=1)
  YmY=matrix(Y,ncol=1)%*%matrix(rep(1,npat),nrow=1)-matrix(rep(1,npat),ncol=1)%*%matrix(Y,nrow=1)
  gamest=(tanh(3*XmX)+tanh(3*YmY))/2
  dgamestX=(3*dtanh(3*XmX))/2
  dgamestY=(3*dtanh(3*YmY))/2
  FiFj <- Freq %*% t(Freq)
  rowDifX=rowSums(2*FiFj*(gamest-gam)*dgamestX)
  rowDifY=rowSums(2*FiFj*(gamest-gam)*dgamestY)
  2*c(rowDifX,rowDifY)
  }
##

#run through the four possible starting values and see which one leads to the best result
for (sv in 1:4){
X=startvals[[sv]]$X
Y=startvals[[sv]]$Y
step1XY <- optim(c(X, Y), fn = CritDif, 
                   gr = grCritDif, gam = gam, Freq = freqs, method = "L-BFGS-B", 
                   lower = -18, upper = 18, control = list(maxit = 1000, trace = 1))
crit1 = realcriteria(c(step1XY$par[1:npat], step1XY$par[-(1:npat)])
                        , gam, Freq = freqs)$overall$pccorrect
if(crit1>bestcrit){
	bestX=step1XY$par[1:npat]
	bestY=step1XY$par[-(1:npat)]
	bestcrit=crit1
	}
}
X <- rank(bestX,ties.method="first")
Y <- rank(bestY,ties.method="last")

##GREEDY CONTINUATION
if(greedy==TRUE){
stop=FALSE
XCURR=X
YCURR=Y
while(stop==FALSE){
moves2=movecheck(XCURR,YCURR,patmat,freqs)
print(moves2$BestMove$pccorrect)
if(moves2$CurrentCorrect==moves2$BestMove$pccorrect){stop=TRUE}
XCURR=rank(replace(XCURR,moves2$BestMove$Case,moves2$BestMove$NewX))
YCURR=rank(replace(YCURR,moves2$BestMove$Case,moves2$BestMove$NewY))
	}
#update final POSAC results
X=XCURR
Y=YCURR
}
##END OF GREEDY CONTINUATION

X <- rank(X,ties.method="first")
Y <- rank(Y,ties.method="last")
crits = realcriteria(c(X, Y), gam, Freq = freqs)

rescX = round(100 * (X - min(X))/(max(X) - min(X)))
rescY = round(100 * (Y - min(Y))/(max(Y) - min(Y)))
tidyframe = data.frame(Structuple = pattern, freqs = freqs, 
                       X = X, Y = Y, rescX = rescX, rescY = rescY, J = rescX + 
                         rescY, L = 100 + rescX - rescY)

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

