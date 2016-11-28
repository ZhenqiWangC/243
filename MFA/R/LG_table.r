
#' @title Lg Table
#' @description Return a table of Lg coefficients between any two subsets of a normalized dataset
#' @param dataset a normalized dataframe or matrix
#' @param sets list of vector contains vector of indices of each group
#' @return a table of Lg coefficients
#' @export
#' @examples
#' # default 
#' nadtas <- scale(wine_data)
#' LG_table(ndatas,sets=list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53))

LG_table <- function(dataset,sets){
  if(!is.data.frame(dataset)&!is.matrix(dataset)){stop("dataset must be a matrix or a dataframe")}
    LG <- matrix(NA,length(sets),length(sets))
    for(i in 1:length(sets)){
      for(j in i:length(sets)){
        lg <- LG(as.matrix(dataset[,sets[[i]]]),as.matrix(dataset[,sets[[j]]]))
        LG[i,j] <- lg
        LG[j,i] <- lg
      }
    }
    LG
}