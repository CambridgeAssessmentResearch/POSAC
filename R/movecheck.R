#' Function to explore whether moving the (X,Y) location of any single pattern may improve POSAC performance
#'
#' This function takes an existing set of X and Y estimates, a matrix of patterns, and the frequencies of the patterns and examines whether
#' moving the (X,Y) coordinates of any one pattern may improve the criterion value that POSAC attempts to optimise.
#' As such this function can be used as one check of whether the results from the POSAC function itself are optimal.
#' Note that this function works by an exhaustive search for all patterns across the grid of all possible positionings.
#' As such, it is quite slow and probably only worth applying once a fairly good initial solution has been found.
#' 
#' @param X The initial X values assigned to each pattern.
#' @param Y The initial Y values assigned to each pattern.
#' @param patmat A matrix of patterns of values across. Each row of the matrix should represents a distinct pattern with no duplicates. The columns are the variables defining the patterns.
#' @param freqs A vector of frequencies of each pattern. This should have the same length as nrow(patmat). 
#'
#' @return The function returns a list with the following elements:
#' \describe{
#'   \item{CurrentCorrect}{The percentage of pairs correctly mapped by the POSAC function to begin with.}
#'   \item{BestMove}{Details of the best possible move including an updated per cent correct. This need not be above the current value.}
#' }
#'
#' @keywords Scalogram
#' @export
#' @examples
#' \dontrun{
#' #Look at whether initial POSAC solution for CRASdata can be improved
#' posac2=POSAC(CRASdata[,1:5],CRASdata[,6])
#' Sys.time()
#' moves=movecheck(posac2$X,posac2$Y,posac2$patmat,posac2$freq)
#' Sys.time()
#' moves
#' #now update the posac analysis to reflect improved results
#' newX=rank(replace(posac2$X,moves$BestMove$Case,moves$BestMove$NewX))
#' newY=rank(replace(posac2$Y,moves$BestMove$Case,moves$BestMove$NewY))
#' posac3=POSACupdate(posac2,newX,newY)
#' posac3
#' #recheck (slow)
#' Sys.time()
#' moves2=movecheck(posac3$X,posac3$Y,posac3$patmat,posac3$freq)
#' Sys.time()
#' moves2

#' #try an iterative approach to update everything until no improvement possible (NOTE: very slow to run)
#' stop=FALSE
#' XCURR=posac2$X
#' YCURR=posac2$Y
#' print(Sys.time())
#' while(stop==FALSE){
#' moves2=movecheck(XCURR,YCURR,posac2$patmat,posac2$freq)
#' print(Sys.time())
#' print(moves2$BestMove$pccorrect)
#' if(moves2$CurrentCorrect==moves2$BestMove$pccorrect){stop=TRUE}
#' XCURR=rank(replace(XCURR,moves2$BestMove$Case,moves2$BestMove$NewX))
#' YCURR=rank(replace(YCURR,moves2$BestMove$Case,moves2$BestMove$NewY))
#' }
#' #update final POSAC results again
#' posac3=POSACupdate(posac2,XCURR,YCURR)
#'  }


movecheck=function(X,Y,patmat,freqs){
  
  if(!(length(X)==length(Y) & length(X)==length(freqs) & length(X)==nrow(patmat))){
    print("length(X), length(Y), length(freqs), and nrow(patmat) must all be equal")
    return(NULL)}
  
  gam<-gamfunc(patmat)
  
  crits=realcriteria(c(X,Y),gam,Freq=freqs,printwarn=FALSE)$overall$pccorrect
  
  possXs=c(min(X)-1,sort(unique(X)),max(X)+1)
  possXs=0.5*(possXs[2:length(possXs)]+possXs[1:(length(possXs)-1)])
  possYs=c(min(Y)-1,sort(unique(Y)),max(Y)+1)
  possYs=0.5*(possYs[2:length(possYs)]+possYs[1:(length(possYs)-1)])
  
  possmoves=expand.grid(1:length(X),possXs,possYs)

  func1=function(row,XA,YA,gam,freqs){
    X1=replace(XA,possmoves[row,1],possmoves[row,2])
    Y1=replace(YA,possmoves[row,1],possmoves[row,3])
    realcriteria(c(X1,Y1),gam=gam,Freq=freqs,printwarn=FALSE)$overall$pccorrect
  }
  #func1(1,XA=X,YA=Y,gam=gam,freqs=freqs)
  
  moveresults=sapply(1:nrow(possmoves),func1,XA=X,YA=Y,gam=gam,freqs=freqs)
  
  possmoves$pccorrect=moveresults
  possmoves=possmoves[order(-moveresults),]
  head(possmoves)
  names(possmoves)[1:3]=c("Case","NewX","NewY")
  BestMove=possmoves[1,]
  CurrentCorrect=crits
  BestMove$OldX=X[BestMove[1,1]]
  BestMove$OldY=Y[BestMove[1,1]]
  
  return(list(CurrentCorrect=CurrentCorrect,BestMove=BestMove))}


