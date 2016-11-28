
#' @title Rv Table
#' @description Return a table of Rv coefficients between any two subsets of a normalized dataset
#' @param dataset a normalized dataframe or matrix
#' @param sets list of vector contains vector of indices of each group
#' @return a table of Rv coefficients
#' @export
#' @examples
#' # default 
#' nadtas <- scale(wine_data)
#' RV_table(ndatas,sets=list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53))
RV_table <- function(dataset,sets){
  if(!is.data.frame(dataset)&!is.matrix(dataset)){stop("dataset must be a matrix or a dataframe")}
    RV <- matrix(NA,length(sets),length(sets))
    for(i in 1:length(sets)){
      for(j in i:length(sets)){
        rv <- RV(as.matrix(dataset[,sets[[i]]]),as.matrix(dataset[,sets[[j]]]))
        RV[i,j] <- rv
        RV[j,i] <- rv
      }
    }
    RV
}