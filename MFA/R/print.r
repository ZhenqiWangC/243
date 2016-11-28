#' @export

# set print() to print basic infomation
setMethod("print",
  signature="mfa",
  function(x,...){
    cat(paste("There are",length(x@eigenvalues),"components."),"\n")
    cat("The eigenvalue of the first component is: ",  x@eigenvalues[1],"\n")
    cat("The eigenvalue of the second component is: ",  x@eigenvalues[2],"\n")
  }
)