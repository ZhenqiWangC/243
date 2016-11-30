
#' @title Lg Coefficients
#' @description Return a value of Lg coefficient between two tables
#' @param table1 a normalized data matrix
#' @param table2 a normalized data matrix
#' @return a value of Lg coefficient between two tables
#' @export
#' @examples
#' # default
#' x1 <- scale(wine_data[,1:6])
#' x2 <- scale(wine_data[,7:12])
#' LG(x1,x2)

# set funtion LG() to take two tables and return lg coefficient
LG<-function(table1,table2){
    #To check input data
    if(!is.numeric(table1)) {stop("data should be numeric matrix or data.frame")}
    if(!is.numeric(table2)) {stop("data should be numeric matrix or data.frame")}
    
    t1 <- t(table1)
    t2 <- t(table2)
    gamma1<-svd(table1)$d[1]
    gamma2<-svd(table2)$d[1]
    sum(diag((table1 %*% t1) %*% (table2 %*% t2)))/(gamma1^2*gamma2^2)
    
}