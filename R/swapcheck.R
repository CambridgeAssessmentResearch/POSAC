#' Function to explore whether swapping the location of any two patterns may improve POSAC performance
#'
#' This function takes an existing set of X and Y estimates, a matrix of patterns, and the frequencies of the patterns and examines whether
#' swapping the (X,Y) coordinates of any two patterns may improve the criterion value that POSAC attempts to optimise.
#' As such this function can be used as one check of whether the results from the POSAC function itself are optimal.
#' 
#' @param X The initial X values assigned to each pattern.
#' @param Y The initial Y values assigned to each pattern.
#' @param patmat A matrix of patterns of values across. Each row of the matrix should represents a distinct pattern with no duplicates. The columns are the variables defining the patterns.
#' @param freqs A vector of frequencies of each pattern. This should have the same length as nrow(patmat). 
#'
#' @return The function returns a list with the following elements:
#' \describe{
#'   \item{CurrentCorrect}{The percentage of pairs correctly mapped by the POSAC function to begin with.}
#'   \item{BestSwap}{Details of the best possible swap including an updated per cent correct. This need not be above the current value.}
#' }
#'
#' @keywords Scalogram
#' @export
#' @examples
#' posac2=POSAC(CRASdata[,1:5],CRASdata[,6])
#' swapcheck(posac2$X,posac2$Y,posac2$patmat,posac2$freqs)
#' #no improvement - however it is possible to swap some incomparable patterns without damaging criteria
#'
#'
#' #showing how successive looking for swaps could be used as a (very poor) alternative algorithm
#' randX=rank(rnorm(nrow(CRASdata)))
#' randY=rank(rnorm(nrow(CRASdata)))
#' swap1=swapcheck(randX,randY,CRASdata[,1:5],CRASdata[,6])
#' swap1
#' #make the suggested swaps
#' randX=replace(randX,sort(as.numeric(swap1$BestSwap[1,1:2])),randX[as.numeric(swap1$BestSwap[1,1:2])])
#' randY=replace(randY,sort(as.numeric(swap1$BestSwap[1,1:2])),randY[as.numeric(swap1$BestSwap[1,1:2])])
#' #check for any more
#' swap2=swapcheck(randX,randY,CRASdata[,1:5],CRASdata[,6])
#' swap2
#' #and so on...

swapcheck=function(X,Y,patmat,freqs){

if(!(length(X)==length(Y) & length(X)==length(freqs) & length(X)==nrow(patmat))){
	print("length(X), length(Y), length(freqs), and nrow(patmat) must all be equal")
	return(NULL)}
	
gam<-gamfunc(patmat)

crits=realcriteria(c(X,Y),gam,Freq=freqs,printwarn=FALSE)$overall$pccorrect

possswaps=expand.grid(1:length(X),1:length(X))
possswaps=possswaps[possswaps$Var1>possswaps$Var2,]

func1=function(row,XA,YA,gam,freqs){
  X1=replace(XA,sort(as.numeric(possswaps[row,])),XA[as.numeric(possswaps[row,])])
  Y1=replace(YA,sort(as.numeric(possswaps[row,])),YA[as.numeric(possswaps[row,])])
  realcriteria(c(X1,Y1),gam=gam,Freq=freqs,printwarn=FALSE)$overall$pccorrect
}

swapresults=sapply(1:nrow(possswaps),func1,XA=X,YA=Y,gam=gam,freqs=freqs)

possswaps$pccorrect=swapresults
possswaps=possswaps[order(-swapresults),]
possswaps
names(possswaps)[1:2]=c("SwapThis","WithThat")
BestSwap=possswaps[1,]
CurrentCorrect=crits

return(list(CurrentCorrect=CurrentCorrect,BestSwap=BestSwap))}

