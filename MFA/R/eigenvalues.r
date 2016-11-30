#' @include mfa.r
#' @title Eigenvalues
#' @description Returns a table with summarizing information about the obtained eigenvalues.
#' @param object an \code{mfa} object
#' @return a dafa frame contains:
#' @return \item{Singular value}{The square root of eigenvalues}
#' @return \item{Eigenvalue}{The eigenvalues of \code{mfa} object}
#' @return \item{Cumulative}{Cumulative sum of eigenvalues}
#' @return \item{% Inertia}{Percentage inertia of eigenvalues}
#' @return \item{Cumulative % Inertia}{Cumulative percentage inertia of eigenvalues}
#' @export
#' @example 
#' 
# set eigenvalues() to take 'mfa' and return a table (like Table 2)
setGeneric("eigenvalues",function(object) standardGeneric("eigenvalues"))

#' @title Eigenvalues Method for \code{mfa} Object
#' @description Returns a table with summarizing information about the obtained eigenvalues.
#' @param object an \code{mfa} object
#' @return a dafa frame contains:
#' @return \item{Singular value}{The square root of eigenvalues}
#' @return \item{Eigenvalue}{The eigenvalues of \code{mfa} object}
#' @return \item{Cumulative}{Cumulative sum of eigenvalues}
#' @return \item{% Inertia}{Percentage inertia of eigenvalues}
#' @return \item{Cumulative % Inertia}{Cumulative percentage inertia of eigenvalues}
#' @export
#' @example 
#' 
setMethod("eigenvalues",signature="mfa",
  function(object){
    eigenvalue <- object@eigenvalues
    singular_value <- sqrt(eigenvalue)
    cumulative <- cumsum(eigenvalue)
    inertia <- eigenvalue/sum(eigenvalue)*100
    cumulative_precentage <- cumulative/sum(eigenvalue)*100
    
    df <- data.frame(rbind(singular_value,eigenvalue,cumulative,inertia,cumulative_precentage))
    colnames(df) <- 1:length(eigenvalue)
    rownames(df) <- c("Singular value", "Eigenvalue","Cumulative","% Inertia","Cumulative % Inertia")
    df
  }
)
