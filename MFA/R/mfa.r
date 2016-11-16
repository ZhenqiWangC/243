X: the whole data
X<-data.matrix(data)

# Construct S=XAXt
S<-as(X %*% A %*% t(X),"matrix")

# do spectral decomposition on S: S=P*LAMBDA*Pt, PtMP=I
# construct inverse delta: delta^2=LAMBDA
eigens<-eigen(S)
d<-matrix(0,dim(X)[1],dim(X)[1])
for (i in 1:length(eigens$values)){
    d[i,i]<-eigens$values[i]
}
u<-eigens$vectors
print(eigens$values)
lambda<-as(M_half %*% d %*% M_half,"matrix")

delta_value<-diag(as(sqrt(lambda),"matrix"))

delta_inv<-1/sqrt(lambda)
delta_inv[is.infinite(delta_inv)]<-0

# P is PMPt=I FOR S=P*LAMBDA*Pt
P <- as(u %*% M_half_inv,"matrix")
print("P")
print(P[,1:2])
# Q FOR Q=Xt*M*P*DELTA_inverse
Q <- as(t(X) %*% M %*% P %*% delta_inv, "matrix")



# build a list: 'partial_factor_score' to store partial factor score
# build a matrix: 'common_factor_score' to store common factor score
# the partial score of group i is named "Partial Score: Group i"
# partial factor score i = no. of group * A_i* data group i* Q_i
# common factor score = sum of partial factor score i


partial_factor_score<-list()
common_factor_score<-0
for (i in 1:length(sets)){
    datai<- data.matrix(eval(parse(text=paste0("Group",i))))
    score<-length(sets) * (1/singularvalues[i]^2) * datai %*% t(datai)  %*% M %*% P %*% delta_inv
    partial_factor_score[[paste0("Partial Score: Group ",i)]]=as(score,"matrix")
    common_factor_score<-score+common_factor_score
}
common_factor_score<-common_factor_score/length(sets)


# loading uses Q
new (Class = "mfa",
eigenvalues = delta_value^2,
common_factor_score = as(common_factor_score,"matrix"),
partial_factor_score = partial_factor_score,
loadings = as(Q,"matrix")
)
}

######################### test ###################
#load wine data
data<-read.csv("wine.csv",header=T,stringsAsFactors=F)
datas<-data[,2:54]
ndatas<-apply(datas,2,function(x){ (x-mean(x))/norm(x-mean(x),type="2")})
test<-mfa(ndatas,sets=c(6,6,6,5,6,5,4,6,5,4),center=FALSE,scale=FALSE)


################ supplementary method #############


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

# Bootstrap?