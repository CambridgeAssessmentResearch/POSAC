#' Take a data set of individuals and reduce to a data set of data patterns along with the frequency of each one.
#'
#' This function takes a standard data set and converts it into a form ready for POSAC analysis.
#' The output of the function will be a data frame with one more column that the input data. The final column will be called "freq" and will
#' hold the number of cases with the given pattern.
#'
#' @param data A data frame with each row representing an inidividual and columns representing the variable which make up the patterns of interest.
#' @keywords Scalogram
#' @export
#' @examples
#' d1=data.frame(x1=c(1,1,1,2,2,2,3,3,3)
#'              ,x2=c(1,2,1,1,1,2,2,3,3)
#'              ,x3=c(1,2,2,2,2,3,3,3,3))
#' pats=DataToPatterns(d1)
#' pats
#' POSAC(pats[,1:3],pats[,4])

DataToPatterns=function(data){
agg1=aggregate(data.frame(temp=data[[1]]),by=as.list(data),length)
names(agg1)[dim(agg1)[2]]="freq"
return(agg1)
}
