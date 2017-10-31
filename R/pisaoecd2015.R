#' Performance of OECD countries in PISA 2015.
#'
#' For each of 35 countries, the data set provides 8 measures of performance in each of Mathematics, Reading and Science.
#' See \url{http://www.oecd.org/education/pisa-2015-results-volume-i-9789264266490-en.htm} for more information.
#'
#' @format A data frame with 34 rows and 25 variables:
#' \describe{
#'  \item{Country}{Name of the country}
#'  \item{mathMean}{Mean performance in  Mathematics}
#'  \item{mathpc5}{5th percentile of performance in  Mathematics}
#'  \item{mathpc10}{10th percentile of performance in  Mathematics}
#'  \item{mathpc25}{25th percentile of performance in  Mathematics}
#'  \item{mathpc50}{50th percentile of performance in  Mathematics}
#'  \item{mathpc75}{75th percentile of performance in  Mathematics}
#'  \item{mathpc90}{90th percentile of performance in  Mathematics}
#'  \item{mathpc95}{95th percentile of performance in  Mathematics}
#'  \item{readMean}{Mean performance in  Reading}
#'  \item{readpc5}{5th percentile of performance in  Reading}
#'  \item{readpc10}{10th percentile of performance in  Reading}
#'  \item{readpc25}{25th percentile of performance in  Reading}
#'  \item{readpc50}{50th percentile of performance in  Reading}
#'  \item{readpc75}{75th percentile of performance in  Reading}
#'  \item{readpc90}{90th percentile of performance in  Reading}
#'  \item{readpc95}{95th percentile of performance in  Reading}
#'  \item{sciMean}{Mean performance in  Science}
#'  \item{scipc5}{5th percentile of performance in  Science}
#'  \item{scipc10}{10th percentile of performance in  Science}
#'  \item{scipc25}{25th percentile of performance in  Science}
#'  \item{scipc50}{50th percentile of performance in  Science}
#'  \item{scipc75}{75th percentile of performance in  Science}
#'  \item{scipc90}{90th percentile of performance in  Science}
#'  \item{scipc95}{95th percentile of performance in  Science}
#' }
#' @examples
#' #POSAC analysis of PISA 2015 results
#' dim(pisaoecd2015)
#' #Get an initial solution
#' posacP=POSAC(pisaoecd2015[,2:25],rep(1,dim(pisaoecd2015)[1]))
#' posacP$Criteria
#' plot(posacP$X,posacP$Y)
#' text(posacP$X,posacP$Y,pisaoecd2015$Country)
#'
#' #look for improvements to solution
#' stop=FALSE
#' XCURR=posacP$X
#' YCURR=posacP$Y
#' print(Sys.time())
#' while(stop==FALSE){
#' moves2=movecheck(XCURR,YCURR,posacP$patmat,posacP$freq)
#' print(Sys.time())
#' print(moves2$BestMove$pccorrect)
#' if(moves2$CurrentCorrect==moves2$BestMove$pccorrect){stop=TRUE}
#' XCURR=rank(replace(XCURR,moves2$BestMove$Case,moves2$BestMove$NewX))
#' YCURR=rank(replace(YCURR,moves2$BestMove$Case,moves2$BestMove$NewY))
#' }
#' #update final POSAC results
#' posacP3=POSACupdate(posacP,XCURR,YCURR)
#' posacP3$Criteria
#' plot(posacP3$X,posacP3$Y,xlab="X value from POSAC of PISA 2015 performance",ylab="Y value from POSAC of PISA 2015 performance")
#' text(posacP3$X,posacP3$Y,pisaoecd2015$Country,cex=0.8)
#'
#' #check there are no swaps that could improve the criteria
#' swapcheck(posacP3$X,posacP3$Y,posacP3$patmat,posacP3$freq)
#' #no
#'
#' #replace columns with ranks and make matrix ready for compare.profiles
#' pisamat=as.matrix(pisaoecd2015[,2:25])
#' for (i in 1:24){pisamat[,i]=rank(pisamat[,i])}
#' #sort by other information by sum of ranks to match with order from compare.profiles
#' ord1=rowSums(pisamat)
#' pisamat=pisamat[order(ord1),]
#' X1=posacP3$X[order(ord1)]
#' Y1=posacP3$Y[order(ord1)]
#' labs=as.character(pisaoecd2015$Country)[order(ord1)]
#' CompPairMat=compare.profiles(pisamat)
#' rownames(CompPairMat)=labs
#' colnames(CompPairMat)=labs
#' hasseDiagram::hasse(CompPairMat,labels=labs)
#' #quite difficult to read
#'
#' #use ggplot2 to look at this in an alternative way
#' #first make a list of edges (where one country universally outperforms another)
#' library(foreach)
#' edges1=foreach(i=1:dim(CompPairMat)[1],.combine=rbind)%do%{
#' 	foreach(j=1:dim(CompPairMat)[1],.combine=rbind)%do%{
#' 	next1=NULL
#' 	if(CompPairMat[i,j]==TRUE){next1=c(i,j)}
#' 	next1}}
#'
#' #remove redundant edges 
#' #(e.g. the UK outperforms Hungary and Hungary outperforms 
#' #Mexico so no need to also include a line between Mexico and the UK)
#' edges1a=foreach(checkj=unique(edges1[,2]),.combine=rbind)%do%{
#' 	edges2=edges1[edges1[,2]==checkj,]
#' 	childlist=unique(edges1[edges1[,2]%in%edges2[,1],1])
#' 	edges2=cbind(edges2,rep(0,dim(edges2)[1]))
#' 	edges2[edges2[,1]%in%childlist,3]=1
#' 	edges2[edges2[,3]==0,1:2]}
#'
#' #now the plot
#' library(ggplot2)
#' df1=data.frame(x=X1,y=Y1,lab=labs)
#' g1=ggplot()+geom_text(data=df1,aes(x=x,y=y,label=lab))+labs(x="X value from POSAC analysis of PISA data",y="Y value from POSAC analysis of PISA data")
#' g1=g1+geom_point(data=df1,aes(x=x,y=y))
#' df2=data.frame(startx=X1[edges1a[,1]]
#' 	,endx=X1[edges1a[,2]]
#' 	,starty=Y1[edges1a[,1]]
#' 	,endy=Y1[edges1a[,2]])
#' g1+geom_segment(data=df2
#' 	,aes(x=startx,xend=endx,y=starty,yend=endy)
#' 	,arrow = arrow(),col="blue",alpha=0.2)
"pisaoecd2015"

