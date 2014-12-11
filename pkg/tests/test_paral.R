require(SensMixed)
checkParallel <- FALSE

if(checkParallel){
  result_nopar <- sensmixed(names(TVbo)[5:ncol(TVbo)], 
                            Prod_effects=c("TVset", "Picture"),
                            replication="Repeat", 
                            individual="Assessor", data=TVbo, 
                            calc_post_hoc = TRUE, parallel = FALSE)
  
  result <- sensmixed(names(TVbo)[5:ncol(TVbo)],
                      Prod_effects=c("TVset", "Picture"),
                      replication="Repeat", individual="Assessor", data=TVbo, 
                      calc_post_hoc = TRUE)
  
  ## results from parallel and NOT parallel should be equal  
  #print(result$fixed$Fval[,])
  stopifnot(all.equal(result_nopar$fixed, result$fixed))
  stopifnot(all.equal(result_nopar$step_res, result$step_res))
  
  
  ##check parallel for MAM
  res1_par <- sensmixed(names(TVbo)[5:ncol(TVbo)],
                        Prod_effects=c("TVset", "Picture"),
                        replication="Repeat", 
                        individual="Assessor", data=TVbo, 
                        calc_post_hoc = TRUE, MAM = TRUE)
  
  res1 <- sensmixed(names(TVbo)[5:ncol(TVbo)],
                    Prod_effects=c("TVset", "Picture"),
                    replication="Repeat", 
                    individual="Assessor", data=TVbo, 
                    calc_post_hoc = TRUE, MAM = TRUE, parallel = FALSE)
  
  stopifnot(all.equal(res1$step_res, res1_par$step_res))
  stopifnot(all.equal(res1$fixed, res1_par$fixed))
  stopifnot(all.equal(res1$random, res1_par$random))
  stopifnot(all.equal(res1$scaling, res1_par$scaling))
}


