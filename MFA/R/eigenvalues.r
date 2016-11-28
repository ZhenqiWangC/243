#' @include mfa.r
#' @title eigenvalues method
#' @description Returns a table with summarizing information about the obtained eigenvalues.
#' @param object an mfa object
#' @export
# set eigenvalues() to take 'mfa' and return a table (like Table 2)
setGeneric("eigenvalues",function(object) standardGeneric("eigenvalues"))

#' @export
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
