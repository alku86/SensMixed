
sensmixed <- function(attributes=NULL, Prod_effects, replication = NULL,
                      individual, data, product_structure = 3,
                      error_structure = "No_Rep", MAM = FALSE, 
                      mult.scaling = FALSE, MAM_PER = FALSE, 
                      adjustedMAM = FALSE, alpha_conditionalMAM = 1, 
                      calc_post_hoc = FALSE, parallel = FALSE,
                      reduce.random = TRUE, alpha.random = 0.1,
                      alpha.fixed = 0.05, interact.symbol = ":", ...)
{  
  result <- sensmixedFun(attributes = attributes , Prod_effects, replication, 
                         individual, data,
                         product_structure = product_structure, 
                         error_structure = error_structure,  MAM = MAM, 
                         mult.scaling = mult.scaling,
                         MAM_PER = MAM_PER, adjustedMAM = adjustedMAM, 
                         alpha_conditionalMAM = alpha_conditionalMAM,
                         calc_post_hoc = calc_post_hoc,
                         parallel = parallel, reduce.random = reduce.random, 
                         alpha.random = alpha.random, 
                         alpha.fixed = alpha.fixed, 
                         interact.symbol = interact.symbol)
  class(result) <- "sensmixed"
  result
}


print.sensmixed <- function(x, ...)
{
  ## output for the random effects
#   colnames.out <- rownames(x$rand$Chi)
#   names <- colnames(x$rand$Chi)
#   tr_rand <- vector("list", length(colnames.out))
#   
#   for(i in 1:length(colnames.out)){       
#     tr_rand[[i]] <- createTexreg(
#       coef.names = names, se=x$rand$Chi[i,],
#       coef = x$rand$Chi[i,],
#       pvalues = x$rand$pvalueChi[i,], isRand=TRUE    
#     )     
#   }
  tr_rand <- .changeOutput(x$random$Chi, x$random$pvalueChi, isRand = TRUE)
  cat("\nTests for the random effects:\n")
  screenreg(tr_rand, custom.model.names = names(tr_rand) )
  
  ## output for the fixed effects
#   colnames.out <- rownames(x$fixed$Fval)
#   names <- colnames(x$fixed$Fval)
#   tr <- vector("list", length(colnames.out))
#   
#   for(i in 1:length(colnames.out)){       
#     tr[[i]] <- createTexreg(
#       coef.names = names, se=x$fixed$Fval[i,],
#       coef = x$fixed$Fval[i,],
#       pvalues = x$fixed$pvalueF[i,], isRand = FALSE    
#     )     
#   }
  
  tr_fixed <- .changeOutput(x$fixed$Fval, x$fixed$pvalueF, isRand = FALSE)
  cat("\nTests for the fixed effects:\n")
  screenreg(tr_fixed, custom.model.names = names(tr_fixed) )
  
  if("scaling" %in% names(x)){
#     colnames.out <- rownames(x$scaling$FScaling)
#     names <- colnames(x$scaling$FScaling)
#     tr <- vector("list", length(colnames.out))
#     
#     for(i in 1:length(colnames.out)){       
#       tr[[i]] <- createTexreg(
#         coef.names = names, se=x$scaling$FScaling[i,],
#         coef = x$scaling$FScaling[i,],
#         pvalues = x$scaling$pvalueScaling[i,], isRand=FALSE     
#       )     
#     }
    tr_scaling <- .changeOutput(x$scaling$FScaling, x$scaling$pvalueScaling, 
                                FALSE)
    cat("\nTests for the scaling effects:\n")
    screenreg(tr_scaling, custom.model.names = names(tr_scaling) )
  }
  
  ## output for the fixed effects
#   names <- colnames(x$fixed$Fval)
#   co1 <- res_paral$fixed$Fval[1,]
#   se1 <- res_paral$fixed$Fval[1,]
#   pval1 <- res_paral$fixed$pvalueF[1,]
#   
#   colnames.out <- rownames(res_paral$fixed$Fval)
#   tr <- vector("list", length(colnames.out))
#   
#   for(i in 1:length(colnames.out)){       
#     tr[[i]] <- createTexreg(
#       coef.names = names, se=res_paral$fixed$Fval[i,],
#       coef = res_paral$fixed$Fval[i,],
#       pvalues = res_paral$fixed$pvalueF[i,],     
#     )     
#   }
#   
#   screenreg(tr, custom.model.names = colnames.out )
#   
#   ########
#   cat("\nTests for fixed effects:\n")
#   cat("matrix of F values:\n")
#   print(x$fixed$Fval,2)
#   cat("matrix of p-values:\n")
#   res <- apply(x$fixed$pvalueF,2, format.pval, digits=2)
#   if(class(res) =="character"){
#     res <- t(as.matrix(res, rownames.force = TRUE))
#   }
#   rownames(res) <- rownames(x$fixed$pvalueF)
#   print(res)
#   
#   cat("\nTests for random effects:\n")
#   cat("matrix of Chi values:\n")
#   print(x$rand$Chi,2)  
#   cat("matrix of p-values:\n")
#   res <- apply(x$rand$pvalueChi,2, format.pval, digits=2)
#   rownames(res) <- rownames(x$rand$pvalueChi)
#   print(res)
#   ## if Scaling is present
#   if("scaling" %in% names(x)){
#     cat("\nTests for scaling effects:\n")
#     cat("matrix of F values for scaling effects:\n")
#     print(x$scaling$FScaling,2)
#     cat("matrix of p-values for Scaling effects:\n")
#     res <- apply(x$scaling$pvalueScaling,2, format.pval, digits=2)
#     if(class(res) =="character"){
#       res <- t(as.matrix(res, rownames.force = TRUE))
#     }
#     rownames(res) <- rownames(x$scaling$pvalueScaling)
#     print(res)
#   }    
}  

plot.sensmixed <- function(x, mult = FALSE, dprime = FALSE, sep = FALSE, cex = 2,  
                           interact.symbol = ":", isFixed = TRUE, 
                           isRand = TRUE, isScaling = TRUE, ...)
{
  plotSensMixed(x, mult = mult, dprime = dprime, sep = sep, cex = cex, 
                interact.symbol = interact.symbol, isFixed = isFixed, 
                isRand = isRand, isScaling = isScaling)
}

saveToDoc <- function(x, file = NA, bold = FALSE, append = TRUE)
{
  if(!(class(x) %in% c("sensmixed", "consmixed")))
    stop("x should be of class sensmixed")
  #if(is.na(file))
  #  stop("need to specify file")
  
  if(class(x)=="sensmixed"){
   .createDocOutputSensmixed(x, file = file, bold = bold, append = append)
  }
  if(class(x) == "consmixed"){ 
   .createDocOutputConsmixed(x, file = file, bold = bold, append = append)
  }
}  


consmixed <- function(response, Prod_effects, Cons_effects=NULL, Cons, data, 
                      structure = 3, alpha.random = 0.1, alpha.fixed = 0.05, ...)
{  
  result <- consmixedFun(response=response, Prod_effects, 
                         Cons_effects=Cons_effects,
                         Cons, data, structure = structure, 
                         alpha.random = alpha.random,
                         alpha.fixed = alpha.fixed)
  class(result)<-"consmixed"
  result
}

plot.consmixed <- function(x, main = NULL, cex = 1.4, 
                           which.plot = c("LSMEANS", "DIFF of LSMEANS"),
                           effs = NULL, ...)
{
  st.x <- x
  class(st.x) <- "step"  
  plot(st.x, main = main, cex = cex, 
       which.plot = which.plot,
       effs = effs)
}



print.consmixed <- function(x, ...)
{
  if(!is.null(x$rand.table))
  {
    cat("\nRandom effects:\n") 
    x$rand.table[,"p.value"] <- format.pval(x$rand.table[,"p.value"], 
                                            digits=4, eps=1e-7)
    x$rand.table[,"Chi.sq"] <- round(x$rand.table[,"Chi.sq"],2)
    print(x$rand.table)   
  }
  
  if(nrow(x$anova.table) != 0)
  {
    if(class(x$model) == "lm" | class(x$model) == "gls")
    {
      cat("\nFixed effects:\n")
      print(x$anova.table)
      cat("\nLeast squares means:\n")
      print(x$lsmeans.table) 
      cat("\nFinal model:\n")
      print(x$model)
      return()
    }
    else
    {
      cat("\nFixed effects:\n")
      x$anova.table[,"Pr(>F)"] <- format.pval(x$anova.table[,"Pr(>F)"], 
                                              digits=4, eps=1e-7)
      x$anova.table[,c("Sum Sq","Mean Sq", "F.value")] <- 
        round(x$anova.table[,c("Sum Sq","Mean Sq", "F.value")],4)
      x$anova.table[,"DenDF"] <- round(x$anova.table[,"DenDF"],2)
      print(x$anova.table) 
      
      if(!is.null(x$lsmeans.table))
      {
        cat("\nLeast squares means:\n")
        printCoefmat(x$lsmeans.table, dig.tst=3,
                     tst.ind=c(1:(which(colnames(x$lsmeans.table)=="Estimate")-1),
                               which(colnames(x$lsmeans.table)=="DF")), digits=3,
                     P.values = TRUE, has.Pvalue=TRUE)
      }
      if(!is.null(x$diffs.lsmeans.table))
      {
        cat("\n Differences of LSMEANS:\n")
        printCoefmat(x$diffs.lsmeans.table, dig.tst=1,
                     tst.ind = 
                       c(1:(which(colnames(x$diffs.lsmeans.table)=="Estimate")-1),
                               which(colnames(x$diffs.lsmeans.table)=="DF")), 
                     digits = 3 ,P.values = TRUE, has.Pvalue = TRUE)
      }
      
    }    
  }
  else
    print(x$anova.table)
  cat("\nFinal model:\n")
  print(x$model@call) 
}  