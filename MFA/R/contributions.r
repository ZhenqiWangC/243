#' @include mfa.r
#' @title Contributions
#' @description Returns a list of tables with contribution informations.
#' @param object an \code{mfa} object
#' @return A list of tables:
#' @return \item{observations}{contribution of each observation to each dimension} 
#' @return \item{variables}{contribution of each variable to each dimension} 
#' @return \item{table}{contribution of each table to each dimension} 
#' @export
#' @example 
#' 
setGeneric("contributions",function(object)standardGeneric("contributions"))

#' @title Contributions Method for \code{mfa} Object
#' @description Returns a list of tables with contribution informations.
#' @param object an \code{mfa} object
#' @return A list of tables:
#' @return \item{observations}{contribution of each observation to each dimension} 
#' @return \item{variables}{contribution of each variable to each dimension} 
#' @return \item{table}{contribution of each table to each dimension} 
#' @export
#' @example 
#' 
# set contributions() to take 'mfa' and return a matrix of contributions

setMethod("contributions",
  signature("mfa"),
  function(object){
    
    K <- length(object@partial_factor_score)
    q <- object@loadings
    t <- as.data.frame(table(object@weights))
    f <- object@common_factor_score
    alpha <- unique(object@weights)
    sum <- 0
    ctr_table <- NULL
    ctr_obs <- NULL
    ctr_var <- NULL
    for(i in 1:K){
      l <- t[which(t==alpha[i]),2]
      ctr_var <- rbind(ctr_var,alpha[i]*q[(sum+1):(sum+l),]^2)
      ctr_table <- rbind(ctr_table,apply(alpha[i]*q[(sum+1):(sum+l),]^2,2,sum))
      sum <- sum+l
    }
    
    lambda <- apply(1/nrow(f)*f^2,2,sum)
    ctr_obs <- sweep(1/nrow(f)*f^2,2,STATS = lambda, FUN = "/")
    list(observations=ctr_obs,
         variables=ctr_var,
         table=ctr_table)
  }
)