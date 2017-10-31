#' POSAC (Partial Order Scalogram Analysis with Base Co-ordinates)
#'
#' An R package to carry out POSAC analysis, a technique used in Facet Theory.
#' Broadly the idea is to reduce a data set with many variables down to two so that we can plot the positions of all cases in the data set in 
#' two dimensions.
#' The aim is to find values of the two variables for each case so that if, for a pair of cases, one case has values at least as high as the 
#' other for every variable (and higher in at least one instance), then that case should be plotted above and to the right of the other.
#' If one case has higher values for some variables and the other has higher values for others then the two cases are deemed incomparable.
#' As such, one case should be plotted below and to the right the other.
#' The positions of points are chosen to preserve these rules as best as possible across all pairs of cases.
#'
#' Note that POSAC works best as a small data technique. In particular, if you have a very large number of variables 
#' don't be surprised if analysis fails to produce useful results.
#'
#' @name POSAC-package
#' @docType package
#' @title POSAC (Partial Order Scalogram Analysis with Base Co-ordinates).
#' @author Tom Benton and Tom Bramley, Cambridge Assessment.
#' @references
#' Shye, S. (2009). POSAC in 4 simple steps. 
#' 
#' (\url{https://www.researchgate.net/profile/Samuel_Shye/publication/263932933_PARTIAL_ORDER_SCALOGRAM_ANALYSIS_BY_COORDINATES_POSAC_AS_A_FACET_THEORY_MEASUREMENT_PROCEDURE_HOW_TO_DO_POSAC_IN_FOUR_SIMPLE_STEPS/links/0a85e53c638c645503000000.pdf})
#' @keywords package
NULL
