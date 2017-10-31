#' Function to plot X and Y output from POSAC with colours for different values of given variables
#'
#' This function produces plots of the values of the two variables (X and Y) produced by the analysis (e.g. the POSAC function) against one another. Colour is added to the chart to represent the values of a particular individual variable within the data set. 
#' These plots may help to identify whether a given indidvidual variable is particularly important in the definition of either X or Y.
#' Note that if multiple plots are produced (for multiple items) the positions of points will be identical in every plot. 
#' Only the colouring and labelling of points will vary.
#'  
#'
#' @param posacout Output from function POSAC (or POSACupdate).
#' @param vars Indices of items from pattern matrix. Including too many items at once may possibly produce an error. Also note that if multiple items are specified, the positioning of points will be the same in each plot generated - only the colouring and labelling will change.
#'
#' @keywords Scalogram
#' @export
#' @examples
#' posac2=POSAC(CRASdata[,1:5],CRASdata[,6])
#' itemdiagram(posac2,1)
#' itemdiagram(posac2,2)
#' itemdiagram(posac2,1:3)
#' itemdiagram(posac2,1:5)

itemdiagram <- function (posacout, vars) {
  dim1 = ceiling(sqrt(length(vars)))
  dim2 = ceiling(length(vars)/dim1)
  par(mfrow = c(dim1, dim2))
  
  bluegreyred <- colorRampPalette(c("blue", "wheat1", "red3"))
  
  for (kkz in vars) {
    d1 = data.frame(X = posacout$X, Y = posacout$Y, val = posacout$patmat[, 
                                                                          kkz])
    nunique <- length(unique(d1$val))
    plot(d1$X, d1$Y, pch = 15, col = bluegreyred(nunique)[as.factor(d1$val)], 
         xlab = "X", ylab = "Y")
    text(d1$X, d1$Y, d1$val, pos = 1)
    title(names(posacout$patmat)[kkz])
  }
  par(mfrow = c(1,1))
}

