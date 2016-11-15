
# create a s4 class "mfa"
setClass(
Class="mfa",
slots=list(eigenvalues="numeric",common_factor_score="matrix",partial_factor_score="matrix",loadings="matrix")
)
# constructor function: to construct 'mfa' and run the model
mfa<-function(data,sets,ncomps=NULL,center=TRUE,scale=TRUE){
new (Class = "mfa",
eigenvalues = data,
common_factor_score = matrix(data, nrow = 2, ncol = 2),
partial_factor_score = matrix(data, nrow = 2, ncol = 2),
loadings = matrix(data, nrow = 2, ncol = 2)
)
}
# set print() to print basic infomation
setMethod("print",
signature="mfa",
function(x){x@eigenvalues})
# set plot() to plot table given two dimensions
setGeneric("plot",function(object)standardGeneric("plot"))
setMethod("plot",signature="mfa",
function(object){
}
)
# set eigenvalues() to take 'mfa' and return a table (like Table 2)
setGeneric("eigenvalues",function(object)standardGeneric("eigenvalues"))
setMethod("eigenvalues",signature="mfa",
function(object){
}
)
# set contributions() to take 'mfa' and return a matrix of contributions
setGeneric("contributions",function(object)standardGeneric("contributions"))
setMethod("contributions",signature="mfa",
function(object){
}
)
# set funtion RV() to take two tables and return rv coefficient
RV<-function(table1,table2){}

# set method RV_table() to take 'mfa' dataset, list and return coefficients
setGeneric("RV_table",function(object,sets)standardGeneric("RV_table"))
setMethod("RV_table",signature="mfa",
function(object,sets){
}
)

# set funtion LG() to take two tables and return lg coefficient
LG<-function(table1,table2){}