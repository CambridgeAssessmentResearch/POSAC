#' Function to compare 2 structuples/profiles for 'greater than or equal' relation
#' @param x First structuple/profile
#' @param y Second structuple/profile
#' @export
#' @examples
#' c(1,2)%gte%c(2,1)
#' c(1,2,1,5)%gte%c(1,2,1,2)

"%gte%" <- function(x,y) {
  if (length(x)!=length(y)){
    stop ("Both vectors must be same length.")
  }
  return(all(x>=y))
}

#' Function to compare 2 structuples/profiles for 'less than or equal' relation
#' @param x First structuple/profile
#' @param y Second structuple/profile
#' @export
#' @examples
#' c(1,2)%lte%c(2,1)
#' c(1,2,1,5)%lte%c(11,2,1,7)
"%lte%" <- function(x,y) {
  if (length(x)!=length(y)){
    stop ("Both vectors must be same length.")
  }
  return(all(x<=y))
}

#' Compare each pair of structuples/profiles in a matrix.
#'
#' Compare each pair of structuples/profiles in a matrix.
#' @param pr A matrix where each row is a distinct structuple/profile.
#' @return A logical square matrix with 'true' indicating less than or equal relation for input to HasseDiagram-Hasse function.
#' @examples
#' CompPairMat=compare.profiles(as.matrix(SimplePOSACdata[,1:4]))
#' hasseDiagram::hasse(CompPairMat)
#'
#' CRASCompMat=compare.profiles(as.matrix(CRASdata[,1:5]))
#' #the result of this function can be used to make a Hasse Diagram
#' hasseDiagram::hasse(CRASCompMat)
#' @export

compare.profiles<-function(pr) {
  pr=pr[order(rowSums(pr)),]
  if (!is.matrix(pr)) {
    stop ("Input to function compare.profiles must be a matrix.")
  }
  if (anyDuplicated(pr)!=0) {
    stop ("Input to function compare.profiles must only contain unique profiles.")
  }
  np<-nrow(pr)
  nf<-ncol(pr)
  #create character vector of the distinct profiles to use as labels
  profiles<-paste0(pr[,1])
  for (i in 2:nf) {
    profiles<-paste0(profiles,pr[,i])
  }
  lessthan<-matrix(nrow=np,ncol=np,dimnames=list(profiles,profiles))
  for (i in 1:np) {
    for (j in 1:np) {
      if (i<j & pr[i,] %gte% pr[j,]) {
        stop("Profiles must be in ascending order of total score")
      }
      if (i==j) {
        lessthan[i,j]=FALSE
      } else {
        lessthan[i,j]<-pr[i,] %lte% pr[j,]
      }

    }
  }
  return(lessthan)
}

