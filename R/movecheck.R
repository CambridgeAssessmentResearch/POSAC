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

    gam <- gamfunc(patmat)
    #data frame of current positions
    d0=data.frame(id=1:length(X),X=X,Y=Y,freqs=freqs)
    #possible moves
    possXs = c(min(X) - 1, sort(unique(X)), max(X) + 1)
    possXs = 0.5 * (possXs[2:length(possXs)] + possXs[1:(length(possXs) - 
        1)])
    possYs = c(min(Y) - 1, sort(unique(Y)), max(Y) + 1)
    possYs = 0.5 * (possYs[2:length(possYs)] + possYs[1:(length(possYs) - 
        1)])


#subroutine for looking at each individual case (assuming X,Y,d0, possXs, possYs and gam all exist)
movesubcase=function(case){
d1=d0[d0$id!=case,]
d1$gam1=as.vector(gam[-case,case])
d1$X1=X[case]
d1$Y1=Y[case]
d1$freq1=freqs[case]
d1$gam0=(sign(d1$X-d1$X1)+sign(d1$Y-d1$Y1))/2
d1$gam0=sign(d1$gam0)#(so that different between -1 and +1 is same as between -1 and 0)
currerr=sum(d1$freqs*d1$freq1*(d1$gam1-d1$gam0)^2)
moves=data.frame(id=case,currerr=currerr,besterr=currerr
	,OldX=X[case],OldY=Y[case]
	,NewX=X[case],NewY=Y[case])
if(currerr>0){
d1=merge(d1,data.frame(expand.grid(possXs,possYs)))
d1$gam2=(sign(d1$X-d1$Var1)+sign(d1$Y-d1$Var2))/2
d1$gam2=sign(d1$gam2)#(so that different between -1 and +1 is same as between -1 and 0)
d1$err=d1$freqs*d1$freq1*(d1$gam1-d1$gam2)^2
dagg=aggregate(d1[,"err"],by=list(Var1=d1$Var1,Var2=d1$Var2),sum)
bestrow=which.min(dagg$x)[1]
moves$besterr=dagg$x[bestrow]
moves$NewX=dagg$Var1[bestrow]
moves$NewY=dagg$Var2[bestrow]
}
return(moves)
}

temp1=lapply(1:length(X),movesubcase)
allmoves=do.call(rbind.data.frame,temp1)
allmoves$imp=allmoves$currerr-allmoves$besterr
bestmove=which.max(allmoves$imp)

X1=X
Y1=Y
X1[bestmove]=allmoves$NewX[bestmove]
Y1[bestmove]=allmoves$NewY[bestmove]

return(list(CurrentCorrect=realcriteria(c(X, Y), gam = gam, Freq = freqs, printwarn = FALSE)$overall$pccorrect
	,BestMove=data.frame(Case=bestmove
		,NewX=allmoves$NewX[bestmove]
		,NewY=allmoves$NewY[bestmove]
		,pccorrect=realcriteria(c(X1, Y1), gam = gam, Freq = freqs, printwarn = FALSE)$overall$pccorrect
		,OldX=allmoves$OldX[bestmove]
		,OldY=allmoves$OldY[bestmove])))
	}


