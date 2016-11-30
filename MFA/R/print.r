#'Pringting MFAs
#'
#' @title Pringt Method for \code{mfa} Object
#' @description Pringting an \code{mfa} object.
#' @param x an object of class \code{"mfa"}
#' @return Some summary information about the \code{mfa} object:
#' @return The number of components
#' @return The eigenvalue of the first component
#' @return The eigenvalue of the second component
#' @export
#' @examples 
#' 

# set print() to print basic infomation
setMethod("print",
  signature="mfa",
  function(x,...){
    cat(paste("There are",length(x@eigenvalues),"components."),"\n")
    cat("The eigenvalue of the first component is: ",  x@eigenvalues[1],"\n")
    cat("The eigenvalue of the second component is: ",  x@eigenvalues[2],"\n")
  }
)