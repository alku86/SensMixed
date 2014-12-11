## the file contains the functions used for presenting the results in tables 
## or plots
calc.cols <- function(x)
{
  if(x<0.001) 
    return("red") 
  if(x<0.01) 
    return("orange") 
  if(x<0.05) 
    return("yellow") 
  return("grey")
}



## UNUSED function
.plotFixedPartsSensmixed <- function(Fval, pvalueF, cex=2, interact.symbol){
  #x11()
  #plot.new()
  # layout(matrix(c(rep(2,2),3,rep(2,2),3,rep(2,2),3, rep(1,2),3), 4, 3, 
  #               byrow = TRUE))
  layout(matrix(c(rep(2,2),3,rep(2,2),3,rep(2,2),3, rep(1,2),3), 4, 3, 
                byrow = TRUE), 
         heights=c(0.4, 1 , 1.4), widths = c(2,2,4.3))
  
  #Fval <- resSensMixed$fixed$Fval
  #pvalueF <- resSensMixed$fixed$pvalueF
  
  #### plots for F value
  cex.gr <- cex
  names.fixed <- rownames(Fval)# [inds.fixed]
  if(!interact.symbol==":"){        
    names.fixed <- sapply(names.fixed, change.inter.symbol, interact.symbol)
  }
  
  ylim <- c(0, max(sqrt(Fval)) + 0.5)
  
  names.fixed.effs <- LETTERS[1:nrow(Fval)]
  names.fixed.effs.legend <- paste(names.fixed.effs, collapse="")
  #plot(x=bp1[1,], y=rep(1,15), type="n", axes=F, xlab="", ylab="")
  plot.new()
  #else if (is.matrix(FChi.fvalue))
  #  ylim <- c(0, max(apply(FChi.fvalue,2,sum)))
  for(i in 1:ncol(Fval))
  {
    
    fvals <- matrix(0, nrow(Fval), ncol(Fval))
    fvals[,i] <- sqrt(Fval[,i])
    
    
    if(i == ncol(Fval))
    {
      if(length(colnames(Fval)) > 10)
        cex.names <- cex - 0.2
      else
        cex.names <- cex
      bp <- barplot(fvals, col= unlist(lapply(pvalueF[,i], calc.cols)), 
                    ylim=ylim, las=2, main=expression(paste("Barplot for ",
                                                            sqrt(F), 
                                                            " values"
                    )), names.arg =
                      colnames(Fval), las=2, cex.names=cex.names, beside=TRUE, 
                    add=TRUE, xpd=FALSE, cex.main=cex, cex.axis=cex)       
      #text(x=bp, y=rep(0.5, ncol(Fval)), names.fixed.effs, font=1, cex=cex)
      if(sqrt(max(Fval)) > 8)
        text(x=bp, y=sqrt(Fval)+0.3, names.fixed.effs, font=1, cex=cex)
      else
        text(x=bp, y=sqrt(Fval)+0.15, names.fixed.effs, font=1, cex=cex)
      
      plot.new()        
      legend("right", names.fixed, pch=names.fixed.effs.legend,  
             bty="n", pt.lwd=cex, pt.cex=cex, text.font=1, cex=cex)
      legend("topright", c("ns","p < 0.05", "p < 0.01", "p < 0.001"), pch=15, 
             col=c("grey","yellow","orange","red"), title="Significance", 
             bty="n", cex=cex, text.font=1)
    }
    else{
      if(i==1)
        barplot(fvals, col=unlist(lapply(pvalueF[,i], calc.cols)), 
                ylim=ylim, axes=FALSE, las=2, cex.names=cex, 
                beside=TRUE)    
      else
        barplot(fvals, col=unlist(lapply(pvalueF[,i], calc.cols)), 
                ylim=ylim, axes=FALSE, las=2, cex.names=cex, 
                beside=TRUE, add=TRUE)          
    }       
    if(i < ncol(Fval))
      par(new=TRUE)
  }
  
} 

change.inter.symbol <- function(x, interact.symbol){
  if(grepl(":", x)){
    symb.loc <- substring.location(x, ":")
    spl.effs <- strsplit(x,":")[[1]]
    x <- paste(spl.effs, collapse=interact.symbol)
    return(x)
  }
  x
}

.changeOutput <- function(vals, pvals, isRand){
  colnames.out <- rownames(vals)
  names <- colnames(vals)
  tr <- vector("list", length(colnames.out))
  
  for(i in 1:length(colnames.out)){       
    tr[[i]] <- createTexreg(
      coef.names = names, se=vals[i,],
      coef = vals[i,],
      pvalues = pvals[i,], isRand=isRand)
  }
  
  names(tr) <- colnames.out
  return(tr)
}
# 
# .convertOutputToMatrix <- function(result){
#   resSensMixed$random
#   Chi <- matrix(0, nrow = length(result), ncol = nrow(result[[1]]))
# }

.plotBars <- function(val, pval, title, plotLegend = TRUE, plotLetters = TRUE, 
                      reduceNames = TRUE, cex = 2, cex.main = 2, 
                      ylim = ylim, 
                      names.effs = NULL, 
                      names.effs.legend = NULL){
  if(reduceNames)
    names.arg <- sapply(colnames(val), 
                        function(x) paste(substring(x,1,7),"..", sep=""))
  else
    names.arg <- colnames(val)
  if(plotLegend)
    cex.names <- cex
  else
    cex.names <- cex - 0.7
  for(i in 1:ncol(val))
  {
    vals <- matrix(0, nrow(val), ncol(pval))
    vals[,i] <- val[,i]      
    if(i == ncol(val)){
      if(length(colnames(val)) > 10)
        cex.names <- cex.names - 0.2
      else
        cex.names <- cex.names
      bp <- barplot(vals, col = unlist(lapply(pval[,i], calc.cols)), 
                    ylim=ylim, las = 2, 
                    main = title,
                    names.arg = names.arg, las = 2,
                    cex.names = cex.names, beside = TRUE, add = TRUE, 
                    xpd = FALSE, cex.main = cex.main, cex.axis = cex - 0.3) 
      
      if(plotLetters){
        if(max(val) > 8)
          text(x = bp, y = val + 0.3, names.effs, font = 1, cex = cex - 0.3)
        else
          text(x = bp, y = val + 0.15, names.effs, font = 1, cex = cex - 0.1)
      }
      
      #text(x=bp, y=rep(0.5, ncol(Chi)), names.rand.effs, font=1, cex=cex)
      
      if(plotLegend){
        plot.new()        
        legend("right", rownames(val), pch = names.effs.legend,  
               bty="n", pt.lwd = cex, pt.cex = cex, text.font = 1, 
               cex = cex - 0.4)
        legend("topright", c("ns","p < 0.05", "p < 0.01", "p < 0.001"), pch = 15, 
               col = c("grey","yellow","orange","red"), title = "Significance", 
               bty = "n", cex = cex, text.font = 1)
      }      
      
    }
    else{
      if(i==1)
        barplot(vals,col = unlist(lapply(pval[,i], calc.cols)),
                ylim = ylim, axes = FALSE, las = 2, cex.names = cex, 
                cex.axis = cex, beside = TRUE)    
      else
        barplot(vals,col = unlist(lapply(pval[,i], calc.cols)), 
                ylim = ylim, axes = FALSE, las = 2, cex.names = cex, 
                cex.axis = cex, beside=TRUE, add=TRUE)         
      
    }
    if(i < ncol(val))
      par(new = TRUE)
  }
}

.plotSensMixed <- function(val, pval, title, mult = FALSE, sep = FALSE,
                           cex = 2,                           
                           interact.symbol = ":"){
  ylim <- c(0, max(val) + 0.5)
  
  ## change the interaction symbol
  if(!interact.symbol == ":")      
    rownames(pval) <- rownames(val) <-  sapply(rownames(val), change.inter.symbol, 
                                               interact.symbol)  
  
  
  ## multiple plots
  if(mult){
    reduceNames <- TRUE
    neff <- nrow(val)
    if(sep){
      layout(matrix(c(rep(2,2),3,rep(2,2),3,rep(2,2),3, rep(1,2),3), 4, 3, 
                    byrow = TRUE), 
             heights=c(0.4, 1 , 1.4), widths = c(2,2,4.3))
      reduceNames <- FALSE
    }else{
      if(neff < 2)
        layout( matrix(1:2, 1, 2, byrow=TRUE)) 
      else if(neff < 4)
        layout(matrix(1:4, 2, 2, byrow=TRUE))            
      else if(neff < 5)
        layout(cbind(matrix(1:4, 2, 2, byrow=TRUE), 5:6),
               heights=c(1, 1)) 
      else if(neff < 7)
        layout(cbind(matrix(1:6, 3, 2, byrow=TRUE), 7:9))   
      else if(neff < 10)
        layout(cbind(matrix(1:9, 3, 3, byrow=TRUE), 10:12))  
    }
    
    for(eff in rownames(pval)){
      if(sep){
        plot.new()
        .plotBars(val[eff, , drop=FALSE], pval[eff, , drop=FALSE], 
                  title = eff, plotLegend = FALSE, 
                  plotLetters = FALSE, reduceNames = reduceNames, cex = cex, 
                  cex.main = cex - 0.7, ylim = ylim)
        plot.new()
        legend("right", c("ns","p < 0.05", "p < 0.01", "p < 0.001"), pch=15, 
               col=c("grey","yellow","orange","red"), title="Significance", 
               bty="n", cex = cex - 0.5, text.font=1)
      }
      else{
        .plotBars(val[eff, , drop=FALSE], pval[eff, , drop=FALSE], 
                  title = eff, plotLegend = FALSE, 
                  plotLetters = FALSE, reduceNames = reduceNames, cex = cex, 
                  cex.main = cex - 0.7, ylim = ylim)
      }      
    }
    if(!sep){
      plot.new()
      legend("right", c("ns","p < 0.05", "p < 0.01", "p < 0.001"), pch=15, 
             col=c("grey","yellow","orange","red"), title="Significance", 
             bty="n", cex = cex - 0.5, text.font=1)
    }        
  }else{
    layout(matrix(c(rep(2,2),3,rep(2,2),3,rep(2,2),3, rep(1,2),3), 4, 3, 
                  byrow = TRUE), 
           heights=c(0.4, 1 , 1.4), widths = c(2,2,4.3))
    names.effs <- LETTERS[1:nrow(val)]
    names.effs.legend <- paste(names.effs, collapse="")
    
    plot.new()
    .plotBars(val, pval, title = title, 
              plotLegend = TRUE, plotLetters = TRUE, reduceNames = FALSE, 
              cex = cex, cex.main = cex - 0.2, ylim = ylim,
              names.effs = names.effs, 
              names.effs.legend = names.effs.legend)
  }
}

.changeConsmixedOutputForDoc <- function(table, name.pval){  
  table[, name.pval] <- gsub("<", "&lt ", table[, name.pval])
  table  
}

## output for the sensmixed
.createDocOutputSensmixed <- function(x, file = NA, bold = FALSE, append = TRUE){
  colnames.out.rand <- rownames(x$rand$Chi)
  names <- colnames(x$rand$Chi)
  tr_rand <- vector("list", length(colnames.out.rand))
  
  for(i in 1:length(colnames.out.rand)){       
    tr_rand[[i]] <- createTexreg(
      coef.names = names, se=x$rand$Chi[i,],
      coef = x$rand$Chi[i,],
      pvalues = x$rand$pvalueChi[i,], isRand=TRUE    
    )     
  } 
  
  
  ## output for the fixed effects
  colnames.out.fixed <- rownames(x$fixed$Fval)
  names <- colnames(x$fixed$Fval)
  tr <- vector("list", length(colnames.out.fixed))
  
  for(i in 1:length(colnames.out.fixed)){       
    tr[[i]] <- createTexreg(
      coef.names = names, se=x$fixed$Fval[i,],
      coef = x$fixed$Fval[i,],
      pvalues = x$fixed$pvalueF[i,],
      isRand=FALSE
    )     
  }
  
  if("scaling" %in% names(x)){
    ## output for the scaling  effects if presented
    colnames.out.scaling <- rownames(x$scaling$FScaling)
    caption.scaling <- "F-test for the scaling effects"
    names <- colnames(x$scaling$FScaling)
    tr_scal <- vector("list", length(colnames.out.scaling))
    
    for(i in 1:length(colnames.out.scaling)){       
      tr_scal[[i]] <- createTexreg(
        coef.names = names, se=x$scaling$FScaling[i,],
        coef = x$scaling$FScaling[i,],
        pvalues = x$scaling$pvalueScaling[i,],
        isRand=FALSE
      )     
    }
    regres <- list(lrand = tr_rand, lfixed = tr, lscale = tr_scal)
  }
  else{
    regres <- list(lrand = tr_rand, lfixed = tr)
    colnames.out.scaling <- NULL
    caption.scaling <- NULL
  }
  
  #  wdGet()
  #  funny<-function(){
  #    c <- plot(x, mult = TRUE)
  #    print(c)
  #  }
  #  wdPlot(plotfun=funny,method="bitmap", height = 10 , width = 10)
  #  
  
  if(bold)
    stars <- numeric(0)
  else
    stars <- c(0.001, 
               0.01, 0.05)
  
  custom.model.names =list(
    custom.model.names.rand = colnames.out.rand, 
    custom.model.names.fixed = colnames.out.fixed,
    custom.model.names.scaling = colnames.out.scaling)
  caption = list(
    caption.rand="Likelihood ration test for the random effects",
    caption.fixed="F-test for the fixed effects",
    caption.scaling = caption.scaling)
  htmlreg(regres, 
          file = file, inline.css = FALSE, 
          doctype = FALSE, html.tag = FALSE, head.tag = FALSE, 
          body.tag = FALSE,
          custom.model.names = custom.model.names, 
          caption = caption, caption.above = TRUE, bold=bold,
          stars=stars, append = append)
  
  
#   if(!is.null(x$post_hoc)){
#     if("scaling" %in% names(x))
#       name.pval <- "p.value"
#     else
#       name.pval <- "p-value"
#     #names(x$post_hoc)
#     if(!is.na(file))
#       sink(file = file, append = append)
#     for(i in 1:length(x$post_hoc)){
#       x$post_hoc[[i]][,  name.pval] <- format.pval(x$post_hoc[[i]][, name.pval],
#                                                    digits=3, eps=1e-3)
#       x$post_hoc[[i]] <- .changeConsmixedOutputForDoc(x$post_hoc[[i]],  name.pval)
#       if("scaling" %in% names(x))
#         xt.posthoc <- xtable(x$post_hoc[[i]], align="lccccc",
#                              display=c("s","f", "f", "d", "f", "s"))
#       else
#         xt.posthoc <- xtable(x$post_hoc[[i]], align="lccccccc",
#                              display=c("s","f", "f", "d", "f", "f", "f", "s"))
#       caption(xt.posthoc) <- 
#         paste("Post-hoc for the attribute ", names(x$post_hoc)[i])
#       print(xt.posthoc, caption.placement="top", table.placement="H",
#             sanitize.text.function=function(x){x}, size="\\small", type = "html")
#     }
#     if(!is.na(file))
#       sink()
#   }
  
}

## output for the consmixed
.createDocOutputConsmixed <- function(x, file = NA, bold = FALSE, append = TRUE){
  sink(file = file, append = append)
  
  ## tests for the random effects
  x$rand.table[, "p.value"] <- format.pval(x$rand.table[,"p.value"],
                                           digits=3, eps=1e-3)
  x$rand.table <- .changeConsmixedOutputForDoc(x$rand.table, "p.value")
  if("elim.num" %in% colnames(x$rand.table))
    xt.rand <- xtable(x$rand.table, align="lcccc", 
                      display=c("s","f","d","s","s"))
  else
    xt.rand <- xtable(x$rand.table, align="lccc", 
                      display=c("s","f","d","s"))
  caption(xt.rand) <- "Likelihood ratio tests for the random-effects
  and their order of elimination"
  print(xt.rand, caption.placement="top", table.placement="H",
        sanitize.text.function=function(x){x}, size="\\small", type = "html")
  
  ## tests for the fixed effects
  x$anova.table[, "Pr(>F)"] <- format.pval(x$anova.table[,"Pr(>F)"],
                                           digits=3, eps=1e-3)
  x$anova.table <- .changeConsmixedOutputForDoc(x$anova.table, "Pr(>F)")
  if("elim.num" %in% colnames(x$anova.table)) 
    xt.anova <- xtable(x$anova.table, align="lccccccc",
                       display=c("s","f", "f", "d", "f", "f", "s", "s"))     
  else
    xt.anova <- xtable(x$anova.table, align="lcccccc",
                       display=c("s","f", "f", "d", "f", "f","s"))
  caption(xt.anova) <- 
    "F-tests for the fixed-effects and their order of elimination"
  
  
  print(xt.anova, caption.placement="top", table.placement="H",
        sanitize.text.function=function(x){x}, size="\\small", type = "html")
  
  ## post hoc output
  x$diffs.lsmeans.table[, "p-value"] <- 
    format.pval(x$diffs.lsmeans.table[,"p-value"], digits=3, eps=1e-3)
  x$diffs.lsmeans.table <- 
    .changeConsmixedOutputForDoc(x$diffs.lsmeans.table, "p-value")    
  xt.lsmeans <- xtable(x$diffs.lsmeans.table, align="lccccccc",
                       display=c("s","f", "f", "f", "f", "f","f", "s"))
  caption(xt.lsmeans) <- 
    "Differences of Least Squares Means"
  print(xt.lsmeans, caption.placement="top", table.placement="H",
        sanitize.text.function=function(x){x}, size="\\small", type = "html")
  sink()
}