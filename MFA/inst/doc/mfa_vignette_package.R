## ---- echo = FALSE, message = FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L,width = 100)
library(MFA)

## -------------------------------------------------------------------------------------------------
library(MFA)

## -------------------------------------------------------------------------------------------------
head(wine)[,1:12]

## -------------------------------------------------------------------------------------------------
colnames(wine)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  mfa(data,sets,ncomps=NULL,center=TRUE,scale=TRUE)

## ----results='hide',warning=F,messages=F----------------------------------------------------------
varlist<-list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
mfa_wine<-mfa(wine,sets=varlist,ncomps=7,center=TRUE,scale=TRUE)

## ----results='hide',warnings=F,messages=F---------------------------------------------------------
slotNames(mfa_wine)

## ----warnings=F,messages=F------------------------------------------------------------------------
mfa_wine@eigenvalues

## ----warnings=F,messages=F------------------------------------------------------------------------
mfa_wine@common_factor_score

## ----warnings=F,messages=F------------------------------------------------------------------------
mfa_wine@partial_factor_score[1]

## ----warnings=F,messages=F------------------------------------------------------------------------
head(mfa_wine@loadings)

## -------------------------------------------------------------------------------------------------
print(mfa_wine)

## ---- out.width = '720px', out.length = '700px',dpi=500,fig.asp=0.8,fig.width=9,fig.length=5,fig.align='center',message=F,results='hide'----
plot(mfa_wine,dim=c(1,2),singleoutput=NULL)

## ---- out.width = '500px', out.length = '500px',dpi=500,fig.width=6,fig.asp=0.8,fig.align='center',message=F,results='hide'----
plot(mfa_wine,dim=c(1,2),singleoutput='eig')

## ---- out.width = '500px', out.length = '500px',dpi=500,fig.width=6,fig.asp=0.8,fig.align='center',message=FALSE,results='hide'----
plot(mfa_wine,dim=c(1,2),singleoutput='com')

## ---- out.width = '720px', out.length = '600px',dpi=500,fig.width=9,fig.asp=0.5,message=FALSE,fig.align='right',results='hide'----
plot(mfa_wine,dim=c(1,2),singleoutput='par')

## -------------------------------------------------------------------------------------------------
eigenvalues(mfa_wine)

## -------------------------------------------------------------------------------------------------
contributions(mfa_wine)

