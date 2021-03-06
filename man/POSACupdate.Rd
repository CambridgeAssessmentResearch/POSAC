% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POSACupdate.R
\name{POSACupdate}
\alias{POSACupdate}
\title{Update POSAC output objects given new X and Y (perhaps from movecheck or swapcheck).}
\usage{
POSACupdate(posacout, newX, newY)
}
\arguments{
\item{posacout}{Output from function POSAC (or something with similar parts).}

\item{newX}{Vector of new values for the X co-ordinates (will be converted to ranks).}

\item{newY}{Vector of new values for the Y co-ordinates (will be converted to ranks).}
}
\value{
The function returns a list with the following elements:
\describe{
  \item{Criteria}{The output from the function realcriteria for the new solution.}
  \item{X}{The ranking of each pattern on the first dimension of the returned POSAC solution.}
  \item{Y}{The ranking of each pattern on the second dimension of the returned POSAC solution.}
  \item{Patterns}{A character vector of concatenated patterns.}
  \item{patmat}{The pattern matrix used in analysis to begin with.}
  \item{freqs}{The frequency with which in pattern occurs.}
  \item{tidyframe}{A single data frame containing the structuples(patterns), frequencies, X, Y, rescaled X and Y to be between a 0 and 100, J (=rescaled X+rescaled Y)and L(=100+rescaled X-rescaled Y).}
  \item{mumatrix}{Matrix of monotonicity coefficients between each of the original variables and the resulting rescaled X, Y, J and L.}
}
}
\description{
This function takes the output from the function POSAC along with new vectors of X and Y co-ordinates and updates the object to reflect the change.
}
\examples{
#see help for movecheck function
}
\keyword{Scalogram}
