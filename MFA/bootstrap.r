setGeneric("bootstrp",function(object,nbt=1000)standardGeneric("bootstrap"))
setMethod("bootstrap",signature="mfa",
          function(object,nbt=1000){
          groups<-length(object@partial_factor_score)
          series<-sample(c(seq(1:10)),nbt*groups,TRUE)
          bts<-data.frame(table(series))
          F<-matrix(0,dim(object@partial_factor_score[[1]])[1],dim(object@partial_factor_score[[1]])[2])
          for (i in 1:groups){
            F<-F+bts[i,2]*object@partial_factor_score[[i]]
          }
          MEAN<-F/(nbt*groups)
          F1<-matrix(0,dim(object@partial_factor_score[[1]])[1],dim(object@partial_factor_score[[1]])[2])
          for (i in 1:nbt){
            temp<-matrix(0,dim(object@partial_factor_score[[1]])[1],dim(object@partial_factor_score[[1]])[2])
            for (ii in 1:groups){
              s<-(i-1)*10+ii
              temp<-temp+object@partial_factor_score[[series[s]]]
            }
          F1<-F1+(temp/groups-MEAN)*(temp/groups-MEAN)
          }
          VAR<-F1/nbt
          return(MEAN/sqrt(VAR))
          })

  

            