#' Properly calculated POSAC criteria.
#'
#' This function counts (weighted) number of pairs that are in the correct order in terms of X and Y (realU merely calculates whether differences have been labelled as comparable or incomparable correctly).
#' @param XY A concatenated vector of the the x-coordinates and y-coordinates for each combination of values for the covariates (i.e. should have length 2n if n if the number of combinations we are interested in).
#' @param gam The matrix of gamma values.
#' @param Freq The number of observations with each combination of covariate values.
#' @param printwarn Logical input denoting whether the function should print a message about the number of equal pairs not counted in calculations.
#' @examples
#' posac2=POSAC(CRASdata[,1:5],CRASdata[,6])
#' realcriteria(c(posac2$X,posac2$Y),gamfunc(CRASdata[,1:5]),CRASdata[,6])
#' #Compare to applying random X and Y coordinates
#' realcriteria(c(rnorm(dim(CRASdata)[1]),rnorm(dim(CRASdata)[1])),gamfunc(CRASdata[,1:5]),CRASdata[,6],printwarn=FALSE)
#' @export

realcriteria<-function(XY,gam,Freq,printwarn=TRUE){
	#Create FiFj matrix
	FiFj<-Freq%*%t(Freq)
	#Create X and Y
	nv<-length(XY)/2
	X<-XY[1:nv]
	Y<-XY[(nv+1):(2*nv)]
	#create gamma matrix based on X and Y
	gamXY<-gamfunc(cbind(X,Y))
	Umat<-(gamXY==gam)+0
	diag(Umat)<-rep(0,length(X))
	#print(diag(Umat))
	#Divide by 2 so we don't double count
	U<-sum(FiFj*Umat)/2
	#would also be nice to calculate percentage by comparable and incomparable
	counter<-FiFj
	diag(counter)<-rep(0,length(diag(counter)))
	gam2<-gam^2
	npairs<-sum(counter)/2
	npairscorrect<-U
	npairscomp<-sum(FiFj*gam2)/2	
	npairscompcorrect<-sum(FiFj*Umat*gam2)/2
	gam2reverse<-1-gam2
	diag(gam2reverse)<-rep(0,length(diag(gam2reverse)))
	npairsincomp<-sum(FiFj*gam2reverse)/2	
	npairsincompcorrect<-sum(FiFj*Umat*gam2reverse)/2
	pccorrect<-100*npairscorrect/npairs
	pccompcorrect<-100*npairscompcorrect/npairscomp
	pcincompcorrect<-100*npairsincompcorrect/npairsincomp
	overall<-data.frame(npairs=npairs,npairscorrect=npairscorrect,pccorrect=pccorrect)
	comparable<-data.frame(npairscomp=npairscomp,npairscompcorrect=npairscompcorrect,pccompcorrect=pccompcorrect)
	incomparable<-data.frame(npairsincomp=npairsincomp,npairsincompcorrect=npairsincompcorrect,pcincompcorrect=pcincompcorrect)
	results<-list(overall=overall,comparable=comparable,incomparable=incomparable)

	if(printwarn==TRUE){print(noquote("Equal pairs are not counted in these calculations"))
	nequalpairs<-sum(Freq*(Freq-1)/2)
	cat("There are ",nequalpairs," equal pairs")
	print(noquote(""))
	print(noquote(""))}

	return(results)}

