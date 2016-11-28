
#' @title Rv Coefficients
#' @description Return a value of Rv coefficient between two tables
#' @param table1 a normalized data matrix
#' @param table2 a normalized data matrix
#' @return a value of Rv coefficient between two tables
#' @export
#' @examples
#' # default 
#' x1 <- scale(wine_data[,1:6])
#' x2 <- scale(wine_data[,7:12])
#' RV(x1,x2)

# set funtion RV() to take two tables and return rv coefficient
RV<-function(table1,table2){
	t1 <- t(table1)
	t2 <- t(table2)
  sum(diag((table1 %*% t1) %*% (table2 %*% t2)))/sqrt(sum(diag((table1 %*% t1) %*% (table1 %*% t1)))*sum(diag((table2 %*% t2) %*% (table2 %*% t2))))
}
