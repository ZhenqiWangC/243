
#' @title Rv CoefficientsTable
#' @description Return a table of Rv coefficients between any two subsets of a normalized dataset.
#' @param dataset a normalized dataframe or matrix
#' @param sets list of vector contains vector of indices or variable names of each group
#' @return a table of Rv coefficients
#' @export
#' @examples
#' # default 
#' nadtas <- scale(wine_data)
#' RV_table(ndatas,sets=list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53))
RV_table <- function(dataset,sets){
  if(!is.data.frame(dataset)&!is.matrix(dataset)){stop("dataset must be a matrix or a dataframe.")}
  dataset <-as.matrix(dataset)
  if(!is.numeric(dataset)) {stop("dataset must be a numeric matrix or dataframe.")}
  
  #check sets
  if(is.numeric(sets[[1]])){
    check_sets<-NULL
    for (i in 1:length(sets)) {
      if(!is.numeric(sets[[i]])) {stop("sets should be a list of numeric vectors or character vectors.")}
      check_sets<-c(check_sets,sets[[i]])
    }
    if(length(check_sets)!=ncol(dataset)) {stop("The sum of sets lengths does not equal to the number of columns.")}
    if(any(!check_sets%in%1:ncol(dataset))) {stop("sets out of bounds")}
    if(!identical(1:ncol(dataset),check_sets)) {warning("sets contain some overlapped and skipped columns.")}
  }else{
    if(is.character(sets[[1]])){
      check_sets<-NULL
      check_names<-NULL
      for (i in 1:length(sets)){
        if(!is.character(sets[[i]])) {stop("sets should be a list of character vectors or numeric vectors.")}
        if(any(!sets[[i]]%in%colnames(dataset))) {stop("sets contain wrong variable names.")}
        check_names<-c(check_names,sets[[i]])
        check_sets<-c(check_sets,c(which(colnames(dataset)==sets[[i]][1]):which(colnames(dataset)==sets[[i]][length(sets[[i]])])))
      }
      if(length(check_sets)!=ncol(dataset)) {stop("The sum of sets lengths does not equal to the number of columns, or the variable names are in the wrong order.")}
      if(!identical(colnames(dataset),check_names)) {warning("sets contain some overlapped and skipped columns.")}
    }else{
      stop("sets should be a list of numeric vectors or character vectors.")
    }
  }
  
  # if sets is character: turn sets into indicies acccording to rownames of dataset
  osets<-sets
  if (!is.numeric(sets[[1]])){
    newlist<-list()
    for (i in 1:length(sets)){
      newlist[[i]]<-c(which(colnames(dataset)==sets[[i]][1]):which(colnames(dataset)==sets[[i]][length(sets[[i]])]))
    }
    sets<-newlist
  }  
  
  #computation
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