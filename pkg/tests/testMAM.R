require(SensMixed)
checkMAM <- FALSE

if(checkMAM){
  ## ERROR: bug in MAManalysis: cannot calculate for more than 3 attributes
  atts <- names(TVbo)[7:10]
  res_MAM <- sensmixed(atts, Prod_effects=c("TVset"), 
                       individual="Assessor", data=TVbo, 
                       MAM_PER=TRUE)
  
  ## selecting only 3 attributes
  atts <- names(TVbo)[7:9]
  res <- sensmixed(atts, Prod_effects=c("TVset"), individual="Assessor", 
                   data=TVbo, MAM=TRUE, reduce.random=FALSE, 
                   calc_post_hoc = TRUE)
  
  res_MAM <- sensmixed(atts, Prod_effects=c("TVset"), individual="Assessor", 
                       data=TVbo, MAM_PER=TRUE)
  
  TOL <- 1e-2
  for(i in 1:length(atts)){
    ## ERROR: different results for the Noise attribute
    if(i==1)
      tools::assertError(stopifnot(all.equal(res_MAM[[3]][, , i][2:3,"F"], 
                                             c(res$fixed$Fval[, i],
                                               res$scaling$FScaling[, i]), 
                                             tol=TOL, check.attributes = FALSE)))
    else
      stopifnot(all.equal(res_MAM[[3]][, , i][2:3,"F"], c(res$fixed$Fval[, i],
                                                          res$scaling$FScaling[, i]), 
                          tol=TOL, check.attributes = FALSE))
  }
  
  
  TOL <- 1e-2
  for(i in 1:length(atts)){
    stopifnot(all.equal(res_MAM[[4]][, , i][1, 1], res$post_hoc[[i]][1, 1], tol=TOL, 
                        check.attributes = FALSE))
    stopifnot(all.equal(res_MAM[[4]][, , i][1, 2], res$post_hoc[[i]][2, 1], tol=TOL, 
                        check.attributes = FALSE))
    stopifnot(all.equal(res_MAM[[4]][, , i][3, 2], res$post_hoc[[i]][3, 1], tol=TOL, 
                        check.attributes = FALSE))
  }
  
  ## selecting another attributes to compare
  atts <- names(TVbo)[10:12]
  res <- sensmixed(atts, Prod_effects=c("TVset"), individual="Assessor", 
                   data=TVbo, MAM=TRUE, reduce.random=FALSE, 
                   calc_post_hoc = TRUE)
  
  res_MAM <- sensmixed(atts, Prod_effects=c("TVset"), individual="Assessor", 
                       data=TVbo, MAM_PER=TRUE)
  TOL <- 1e-1
  for(i in 1:length(atts)){
    print(i)
    stopifnot(all.equal(res_MAM[[3]][, , i][2:3,"F"], c(res$fixed$Fval[, i],
                                                        res$scaling$FScaling[, i]), 
                        tol=TOL, check.attributes = FALSE))
  }
  
  TOL <- 1e-1
  for(i in 1:length(atts)){
    stopifnot(all.equal(res_MAM[[4]][, , i][1, 1], res$post_hoc[[i]][1, 1], tol=TOL, 
                        check.attributes = FALSE))
    stopifnot(all.equal(res_MAM[[4]][, , i][1, 2], res$post_hoc[[i]][2, 1], tol=TOL, 
                        check.attributes = FALSE))
    stopifnot(all.equal(res_MAM[[4]][, , i][3, 2], res$post_hoc[[i]][3, 1], tol=TOL, 
                        check.attributes = FALSE))
  }
  
  ## check post-hoc for the multiway case
  atts <- names(TVbo)[5:19]
  res <- sensmixed(atts, Prod_effects=c("TVset", "Picture"), individual="Assessor",                  
                   data=TVbo, MAM=TRUE, calc_post_hoc = TRUE)
  
}
