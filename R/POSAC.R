#' Partial Order Scalogram Analysis with Base Coordinates.
#'
#' This functions takes a matrix of patterns and the frequencies of the patterns and performs POSAC analysis.
#'
#' @param patmat A matrix of patterns of values across. Each row of the matrix should represents a distinct pattern with no duplicates. The columns are the variables defining the patterns.
#' @param freqs A vector of frequencies of each pattern. This should have the same length as nrow(patmat). 
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

POSAC=function(patmat,freqs){
npat=dim(patmat)[1]
#CREATE A MATRIX INDICATING WHETHER PAIRS ARE COMPARABLE OR INCOMPARABLE
#(Gamma Squared)
gam<-gamfunc(patmat)
gam2<-gam^2

#SET STARTING VALUES FOR X AND Y (AVERAGE RANKS AND VECTOR BASED ON ORDER OF DATA SET)
X<-as.vector(rowMeans(apply(patmat,2, rank)))+0.00001*((1:npat)^2)/npat
Y<-0.00001*(1:npat)

#iterate stages of optimisation until complete
stop=0
lastpccorrect=0
while(stop==0){
#first part of optimisation
step1XY<-optim(c(X,Y)/100*max(c(X,Y)), fn=Ufunc, gr = dUfunc,gam2=gam2,Freq=freqs
	,method = "L-BFGS-B",lower=-8,upper=8
	,control=list(fnscale=-1,maxit=300,trace=1))
sortedXY<-resortPOSAC2(step1XY$par,gam,patmat)
X<-rank(sortedXY[1:npat])
Y<-rank(sortedXY[(npat+1):(2*npat)])

#break any ties (say incomparable)
rand1=runif(npat,-0.01,0.01)
X=X+rand1
Y=Y-rand1
X<-rank(X)
Y<-rank(Y)

crit1=realcriteria(c(X,Y),gam,Freq=freqs)$overall$pccorrect
if (crit1==lastpccorrect){stop=1}
lastpccorrect=crit1}

#FINAL STATS
crits=realcriteria(c(X,Y),gam,Freq=freqs)
#PATTERNS
pattern<-apply(patmat,1,paste,collapse="")
#TIDY DATA FRAME

rescX=round(100*(X-min(X))/(max(X)-min(X)))
rescY=round(100*(Y-min(Y))/(max(Y)-min(Y)))

tidyframe=data.frame(Structuple=pattern,freqs=freqs,X=X,Y=Y,rescX=rescX,rescY=rescY,J=rescX+rescY,L=100+rescX-rescY)

return(list(Criteria=crits,X=X,Y=Y,Patterns=pattern,patmat=patmat,freqs=freqs,tidyframe=tidyframe))}


