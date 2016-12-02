context("Test MFA output")

testthat::test_that("TestOutPut",{
  testdata = scale(mtcars)
  testset = list(1:5,6:11)
  ##test LG
  testthat::expect_that(round(LG(testdata,testdata),2),testthat::equals(1.17))
  
  ##test LG_table
  lg_table = matrix(c(1.02,0.98,0.98,1.77),2,2)
  testthat::expect_that(LG_table(testdata,testset),testthat::is_a("matrix"))
  testthat::expect_that(round(LG_table(testdata,testset),2),testthat::equals(lg_table))
  
  ##test RV
  testthat::expect_that(RV(testdata,testdata),testthat::is_a("numeric"))
  testthat::expect_that(RV(testdata,testdata),testthat::equals((1)))
  
  #test RV_table
  rv_table = matrix(c(1,0.73,0.73,1),2,2)
  testthat::expect_that(RV_table(testdata,testset),testthat::is_a("matrix"))
  testthat::expect_that(round(RV_table(testdata,testset),2),testthat::equals(rv_table))
  
  #test bootstrap
  testmfa = mfa(testdata,testset)
  testthat::expect_that(bootstrap(testmfa,nbt = 10),testthat::is_a("matrix"))
  
  #test contributions
  testthat::expect_that(contributions(testmfa),testthat::is_a("list"))
  testthat::expect_that(names(contributions(testmfa)),testthat::equals(c("observations","variables","table")))
  
  #test print
  #testthat::expect_that(print(mfa),testthat::prints_text(NULL))
  
  #test eigenvalues
  testthat::expect_that(eigenvalues(testmfa),testthat::is_a("data.frame"))
  testthat::expect_that(round(eigenvalues(testmfa)[1,1],2),testthat::equals(0.25))
  
  #test wine
  wine = read.csv("MFA/wine.csv")
  sets=list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
  len = sets[[length(sets)]][length(sets[[length(sets)]])]
  datas<-wine[2:length(wine[,1]),2:(len+1)]
  #To make clean data
  ndatas<-apply(datas,2,function(x){ (as.numeric(x)-mean(as.numeric(x)))/norm(as.numeric(x)-mean(as.numeric(x)),type="2")})
  coln<-c()
  for (i in 1:length(sets)){
    coln<-c(coln,paste0(wine[1,sets[[i]]+1],".G",i))
  }
  colnames(ndatas)<-coln
  rownames(ndatas)<-wine[2:length(wine[,1]),1]
  
  ##test LG
  testthat::expect_that(round(LG(ndatas,ndatas),2),testthat::equals(1.07))
  
  ##test LG_table
  wlg_table = matrix(c(1.07,0.92,0.92,1.06),2,2)
  testthat::expect_that(LG_table(ndatas,sets),testthat::is_a("matrix"))
  testthat::expect_that(round(LG_table(ndatas,sets)[1:2,1:2],2),testthat::equals(wlg_table))
  
  #test bootstrap
  wtestmfa = mfa(ndatas,sets)
  testthat::expect_that(bootstrap(wtestmfa,nbt = 10),testthat::is_a("matrix"))
  
  #test contributions
  testthat::expect_that(contributions(wtestmfa),testthat::is_a("list"))
  testthat::expect_that(names(contributions(wtestmfa)),testthat::equals(c("observations","variables","table")))
  
  #test print
  #testthat::expect_that(print(wtestmfa),testthat::prints_text(NULL))
  
  #test eigenvalues
  testthat::expect_that(eigenvalues(wtestmfa),testthat::is_a("data.frame"))
  testthat::expect_that(round(eigenvalues(wtestmfa)[1,1],2),testthat::equals(0.91))
})

