## check if the data is balanced
isbalanced <- function(data)
{
  suppressWarnings(!is.list(replications(~ . , data)))
}

runMAM <- function(data, Prod_effects, individual, attributes, adjustedMAM=FALSE, 
                   alpha_conditionalMAM=1){
  if(length(attributes) < 2)
    stop("number of attributes for MAM should be more than 1")
  if(length(Prod_effects) > 1)
    stop("should be one-way product structure")  
  dataMAM <- data[, c(individual, Prod_effects)]
  dataMAM$replication <- rep(0, nrow(data))
  dataMAM[, 1] <- as.factor(dataMAM[, 1])
  dataMAM[, 2] <- as.factor(dataMAM[, 2])
  if(nlevels(dataMAM[, 2]) < 3)
    stop("There MUST be at least 3 products")
  ## create a rep factor
  assprod <- interaction(dataMAM[, 1], dataMAM[, 2])
  t <- table(assprod)
  if(length(unique(t))!=1)
    stop("data is unbalanced")
  for(i in 1:length(names(t)))
    dataMAM$replication[assprod==names(t)[i]] <- 1:unique(t)
  dataMAM <- cbind(dataMAM, data[, attributes])
  return(MAManalysis(dataMAM, adjustedMAM, alpha_conditionalMAM))
}


### function checks  if there are zero cells in a factor term
checkZeroCell <- function(data, factors)
{
  t <- table(data[, match(factors, names(data))])
  if(length(which(t==0))>0)
  {
    message(paste("Some of the combinations of ", paste(factors,collapse=":"), 
                  " has no data, therefore this combination will not be part of the initial model"))
    cat("\n")
    return(TRUE)
  }
  
  return(FALSE)
}

## checks if the number of levels for an interaction term 
## is equal to number of observations
checkNumberInteract <- function(data, factors)
{
  ## returns TRUE if number of levels is equal to nrow of data
  
  nlev <- 1
  for(i in 1:length(factors))
  {    
    if(!is.factor(data[, match(factors[i], names(data))]))
      next()
    nlev <- nlev * nlevels(data[, match(factors[i], names(data))])
  }
  if(nlev >= nrow(data))
  {
    warning.str <- "Number of levels for "
    if(length(factors) > 1)
      warning.str <- c(warning.str," interaction ", sep=" ")
    #for(i in length(factors))
    #    warning.str <- paste(warning.str, factors[i],sep=" ")  
    warning.str <- c(warning.str, paste(factors,collapse=":"), 
                     " is more or equal to the number of observations in data", 
                     sep=" ")    
    message(warning.str)
    cat("\n")
    return(TRUE)
  }
  return(FALSE)
}


### Function converts variables to factors
convertToFactors <- function(data, facs)
{
  #convert effects to factors
  for(fac in facs)
    data[,fac] <- as.factor(data[,fac])
  data
}

## create formula with only fixed terms
fixedFormula <- function(fmodel)
{
  terms.fm <- attr(terms.formula(fmodel),"term.labels")
  ind.rand.terms <- which(unlist(lapply(terms.fm,
                                        function(x) substring.location(x, "|")$first))!=0)
  terms.fm[ind.rand.terms] <- unlist(lapply(terms.fm[ind.rand.terms],
                                            function(x) paste("(",x,")",sep="")))
  fm <- paste(fmodel)
  fm[3] <- paste(terms.fm[-ind.rand.terms],collapse=" + ")
  if(fm[3]=="")
    fo <- as.formula(paste(fm[2],fm[1],1, sep=""))
  else
    fo <- as.formula(paste(fm[2],fm[1],fm[3], sep=""))
  return(fo)
}


.createFormulaAnovaLsmeans <- function(mf.final, mf.final.lsm, random, fixed,
                                       mult.scaling, data){
  
  ff <- fixedFormula(mf.final)
  ## maximal order of interaction
  max.inter <- max(attr(terms(ff), "order")) 
  scaling.private.adds <- NULL
  if(max.inter > 2){
    scaling.private.adds <- paste("scaling.private.add", 1:(max.inter - 2) , sep="")
    for (i in 1:length(scaling.private.adds)){
      assign(scaling.private.adds[i], rep(1, nrow(data)) )
    }
    #data[, scaling.private.adds] <- rep(1, nrow(data))     
  }
    
  
  
  if(length(fixed$Product)>1 && mult.scaling){
    prods <- paste("x.scaling.private", fixed$Product, sep="")
    for(namesProd in fixed$Product){
      assign(paste("x.scaling.private", namesProd, sep=""), 
             scale(predict(lm(as.formula(paste(ff[2], ff[1], namesProd)), 
                              data=data)), scale=FALSE))
    }
#     lapply(fixed$Product, function(argument) 
#       assign(paste("x.scaling.private", argument, sep=""),
#              scale(predict(lm(as.formula(paste(ff[2], ff[1], argument)), 
#                               data=data)), scale=FALSE))) 
#     data[, prods] <- lapply(paste(ff[2], ff[1], fixed$Product), 
#                             function(formulas)  
#                               scale(predict(lm(as.formula(formulas), 
#                                                data=data)), scale=FALSE) )
  }
  
  # create x out of predicted values from lm
  data$x.scaling.private <- rep(NA, nrow(data))
  x.scaling.private <- scale(predict(lm(ff, data=data)), scale=FALSE)
  notNA <- rownames(x.scaling.private)
  data[notNA, "x.scaling.private"] <- x.scaling.private
  
  ## for anova
  fm <- paste(mf.final)
  if(is.list(random))
    rnd.fo <- random$individual
  else
    rnd.fo <- random

  if(length(fixed$Product) > 1 && mult.scaling){
    if(!is.null(scaling.private.adds))
      fm[3] <- paste(fm[3], paste(rnd.fo, prods, 
                                paste(scaling.private.adds, collapse = ":"), 
                                sep=":", 
                                collapse=" + "), sep =" + ")
    else
      fm[3] <- paste(fm[3], paste(rnd.fo, prods, 
                                  sep=":", 
                                  collapse=" + "), sep =" + ")
  }
  else{
    if(!is.null(scaling.private.adds))
      fm[3] <- paste(fm[3], paste(rnd.fo, 
                                "x.scaling.private", 
                                paste(scaling.private.adds, collapse = ":"),
                                sep=":"), sep=" + ")
    else
      fm[3] <- paste(fm[3], paste(rnd.fo, 
                                  "x.scaling.private",
                                  sep=":"), sep=" + ")
  }

 
  fo.anova <- as.formula(paste(fm[2], fm[1], fm[3], sep=""))    
  
  ## for lsmeans
  fm.lsm <- paste(mf.final.lsm)
  if(is.list(random))
    fm.lsm[3] <- paste(fm.lsm[3],  paste(random$individual, 
                                         "x.scaling.private", sep=":"),
                       "x.scaling.private", sep=" + ")
  else
    fm.lsm[3] <- paste(fm.lsm[3],  paste(random, "x.scaling.private", sep=":"),
                       "x.scaling.private", sep=" + ")
  fo.lsm <- as.formula(paste(fm.lsm[2], fm.lsm[1], fm.lsm[3], sep=""))   
  
  return(list(fo.anova = fo.anova, fo.lsm = fo.lsm, data = data))
  
} 

### Create an lmer model
createLMERmodel <- function(structure, data, response, fixed, random, corr, 
                            MAM = FALSE, mult.scaling = FALSE, 
                            calc_post_hoc = FALSE)
{ 
  
  #construct formula for lmer model    
  mf.final <- createFormulaAllFixRand(structure, data, response, fixed, random, 
                                      corr)    
  ## if MAM needs to be constructed
  if(MAM){
    if(length(fixed$Product)>1){
      data$prod <- interaction(data[, fixed$Product])
      mf.final.lsm <- createFormulaAllFixRand(structure, data, response, 
                                              list(Product="prod", 
                                                   Consumer=fixed$Consumer), 
                                              random, corr)   
    }else{
      mf.final.lsm <- mf.final
    }
    
    ## create formulas for anova and lsmeans   
    ############################################################################
    fo <- .createFormulaAnovaLsmeans(mf.final, mf.final.lsm, random, fixed,
                                     mult.scaling, data)
    
    ## create models for anova and lsmeans
    ############################################################################
    
    ## for anova
    model.anova <- lmerTest::lmer(fo$fo.anova, data)
    #anova(model.anova, type=1) ## TODO: compare with SAS
    #st.anova <- step(model.anova, lsmeans.calc=FALSE, difflsmeans.calc=FALSE, 
    #reduce.fixed=FALSE)
    
    ## change contrasts for lsmeans to be contr.sum
    if(length(fixed$Product)==1){
      mm <- model.matrix(model.anova)
      l <- attr(mm, "contrasts")
      contr <- l
      names.facs <- names(contr)
      l <- as.list(rep("contr.sum", length(names.facs)))
      names(l) <- names(contr)
    }else{
      l <- as.list(rep("contr.sum", 2))
      if(is.list(random))
        names(l) <- c("prod", random$individual) 
      else
        names(l) <- c("prod", random) 
    }   
    
    ## model for lsmeans
    if(calc_post_hoc){
      model.lsmeans <- lmerTest::lmer(fo$fo.lsm, data, contrasts = l)
      return(list(model.anova = model.anova, model.lsmeans = model.lsmeans))
    }
    else return(list(model.anova = model.anova))
    #summaryBy(Coloursaturation ~ prod , data)
    #st.lsmeans <- step(model.lsmeans, lsmeans.calc=FALSE, difflsmeans.calc=FALSE, reduce.fixed=FALSE)
    #newm <- lmerTest::lmer(formula(st.lsmeans$model), data=data, contrasts=l)
    #lsmeans::lsmeans(object=model.lsmeans,  
    #                 pairwise ~ prod)
    #if(length(fixed$Product)==1)
    #  eval(substitute(lsmeans::lsmeans(object=model.lsmeans,  
    #                                 pairwise ~ prod), 
    #                                 list(prod=as.name(fixed$Product))))
    #else
      #eval(substitute(lsmeans::lsmeans(object=model.lsmeans,  
      #                               pairwise ~ prod), 
      #              list(prod=as.name(paste(fixed$Product, collapse=":")))))
    
   #lsmeans::lsmeans(model.lsmeans, pairwise ~ TVset:Picture)
  }else{
    model <- lmerTest::lmer(mf.final, data) 
    return(model)
  }
  
    
  #model <- as(model,"mer")
  #model <- update(model)
  
  #mf.final <- update.formula(formula(model),formula(model))
  #model <- eval(substitute(lmer(mf.final, data=data),list(mf.final=mf.final)))
  #model <- update(model, data=data ,REML=TRUE)
  
  return(model)
}


# check an interaction term for validity
checkComb <- function(data, factors)
{
  return(checkNumberInteract(data,factors) || checkZeroCell(data, factors))
}

.fixedrand <- function(model)
{
  effs <- attr(terms(formula(model)), "term.labels")
  neffs <- length(effs)
  randeffs <- effs[grep(" | ", effs)]
  randeffs <- sapply(randeffs, function(x) substring(x, 5, nchar(x)))
  fixedeffs <- effs[!(effs %in% names(randeffs))]
  return(list(randeffs=randeffs, fixedeffs=fixedeffs))
}

.fillpvalues <- function(x, pvalue)
{
  pvalue[rownames(x$anova.table),x$response] <- x$anova.table[,6]
  pvalue
}

.renameScalingTerm <- function(tableWithScaling, Prod_effects){
  idsScaling <- unlist(lapply(rownames(tableWithScaling), 
                function(x) grepl(":x.scaling.private", x)))
  if(sum(idsScaling) > 1){
    for(i in 1:length(Prod_effects)){
      numscale <- unlist(lapply(rownames(tableWithScaling)[idsScaling], 
                    function(x) grepl(Prod_effects[i], x)))
      rownames(tableWithScaling)[idsScaling][numscale] <- paste("Scaling", 
                                                                Prod_effects[i],
                                                                sep = " ")
    }
  }
  else{
    rownames(tableWithScaling)[unlist(lapply(rownames(tableWithScaling), 
                                             function(x) grepl(":x.scaling.private", x)))] <- 
      "Scaling"
  }
   
  tableWithScaling
}


###############################################################################
# get terms contained  - from lmerTest package
###############################################################################
getIndTermsContained <- function(allterms, ind.hoi)
{
  
  terms.hoi.split <- strsplit(allterms[ind.hoi],":")
  ind.terms.contain <- NULL
  #check which of the terms are contained in the highest order terms
  for(i in (1:length(allterms))[-ind.hoi]) 
  {
    isContained<-FALSE
    for(j in 1:length(terms.hoi.split))
    {
      #if the term is contained in some of the highest order interactions then 
      #we cannot test it for significance
      if(length(which(unlist(strsplit(allterms[i],":")) %in% terms.hoi.split[[j]] == FALSE))==0)
      {
        isContained <- TRUE
        break
      }                
    }
    if(isContained)
      ind.terms.contain <- c(ind.terms.contain,i)
    
  }
  # if there are no terms that are contained in the maximum order effects
  # then compare all the terms between each other for the maximum p value
  if( is.null(ind.terms.contain) )
    return(NULL)
  return(ind.terms.contain)
}

###############################################################################
# get terms contained 
###############################################################################
.getIndTermsContained <- function(allterms, ind.hoi)
{
  
  terms.hoi.split <- strsplit(allterms[ind.hoi],":")
  ind.terms.contain <- NULL
  #check which of the terms are contained in the highest order terms
  for(i in (1:length(allterms))[-ind.hoi]) 
  {
    isContained<-FALSE
    for(j in 1:length(terms.hoi.split))
    {
      #if the term is contained in some of the highest order interactions then 
      #we cannot test it for significance
      if(length(which(unlist(strsplit(allterms[i],":")) %in% terms.hoi.split[[j]] == FALSE))==0)
      {
        isContained <- TRUE
        break
      }                
    }
    if(isContained)
      ind.terms.contain <- c(ind.terms.contain,i)
    
  }
  # if there are no terms that are contained in the maximum order effects
  # then compare all the terms between each other for the maximum p value
  if( is.null(ind.terms.contain) )
    return(NULL)
  return(ind.terms.contain)
}


## get the pure lsmeans for an interaction term
getPureInter <- function(lsm.table, anova.table, eff){

  rows.lsm <- sapply(rownames(lsm.table), 
                     function(x) strsplit(x, " ")[[1]][1]) 
  pure.inter.lsm <- lsm.table[which(rows.lsm %in% eff), ]
  
  contained.effs <- 
    rownames(anova.table)[.getIndTermsContained(rownames(anova.table), 
                                               which(rownames(anova.table) 
                                                     == eff))]
  ## deltas for 3 way interactions
  if( length(unlist(strsplit(eff,":"))) == 3 ){
    ind.inteffs <- grep(":", contained.effs)
    ##plust main effs
    main.effs <- contained.effs[-ind.inteffs]
    ##minus the interactions
    contained.effs <- contained.effs[ind.inteffs]
  }

  p1 <- pure.inter.lsm[ , 1:which(colnames(pure.inter.lsm)=="Estimate") ]
  for(ceff in contained.effs){
    p1  <- merge(p1, 
                 lsm.table[rows.lsm == ceff, 
                           c(unlist(strsplit(ceff,":")), "Estimate")], 
                 by = unlist(strsplit(ceff,":")))
    p1[, "Estimate.x"] <- p1[, "Estimate.x"] - p1[, "Estimate.y"]
    colnames(p1)[which(colnames(p1) == "Estimate.x")] <- "Estimate"
    p1 <- p1[ ,- which(colnames(p1) == "Estimate.y")]        
  }
  ## plus the main effs
  if( length(unlist(strsplit(eff,":"))) == 3 ){
    for(ceff in main.effs){
      p1  <- merge(p1, 
                   lsm.table[rows.lsm == ceff, 
                             c(unlist(strsplit(ceff,":")), "Estimate")], 
                   by = unlist(strsplit(ceff,":")))
      p1[, "Estimate.x"] <- p1[, "Estimate.x"] + p1[, "Estimate.y"]
      colnames(p1)[which(colnames(p1) == "Estimate.x")] <- "Estimate"
      p1 <- p1[ ,- which(colnames(p1) == "Estimate.y")]        
    }
  }
  p1
}

## calculate pure diffs
.calcPureDiffs <- function(pureinter){
  puredifs <- matrix(0, ncol=nrow(pureinter), nrow = nrow(pureinter))
  for (i in 1:nrow(pureinter)) for (j in 1:i) 
    puredifs[i,j] <- pureinter[i, "Estimate"] -  pureinter[j, "Estimate"]  
  puredifs
}

## calculate average d prime from the step function
.calcAvDprime <- function(model, anova.table, dlsm.table, lsm.table){
  sigma <- summary(model, "lme4")$sigma
  rows <- sapply(rownames(dlsm.table), 
                 function(x) strsplit(x, " ")[[1]][1]) 
  anova.table$dprimeav <- rep(1, nrow(anova.table))
  
  for(eff in rownames(anova.table)){
    #lsm <- lsmeans::.old.lsmeans( model , pairwise ~ Track:SPL:Car)
    #dp <- lsm[[2]][, 1]/sigma
    
    
    ## for interaction  - 
    #eff <- attr(terms(model),"term.labels")[3]
    
    #lsm <- lsmeans::.old.lsmeans(model,  as.formula(paste("pairwise ~ ", eff)), 
    #                             lf = TRUE)
    #fixef()
    #split.eff  <-  unlist(strsplit(eff,":"))
    #if( length(split.eff) > 1 ){
      
    pureinter <- getPureInter(lsm.table, anova.table, eff)
    puredifs <- .calcPureDiffs(pureinter) 
      
      
    #all.equal(sum(pure.inter.pairs^2)/(12), sum(puredifs^2)/(12), tol = 1e-4)
    dp <- puredifs / sigma      
    av.dp <- sqrt(sum(dp^2)/(nrow(dp)*(nrow(dp)-1)/2))
    #}
    #else{
     # lsm.eff <- getPureInter(lsm.table, eff)
      
      #dp <- dlsm.table[which(rows %in% eff), 1] / sigma
      #av.dp <- sqrt(sum(dp^2)/length(dp))
      
    #}
    anova.table[eff, "dprimeav"] <- av.dp 
  }
  anova.table
  
  #m.bo <- lme4::lmer(att1 ~ Track*SPL*Car + (1|Assessor) + (1|SPL:Assessor) + 
  #                     (1|Track:SPL:Assessor) + (1|Car:SPL:Assessor), 
  #                   data = sound_data_balanced)
  #lsm <- lsmeans::.old.lsmeans(m.bo,  pairwise ~ Track:SPL:Car, lf = TRUE)
  #lsm[[1]] %*% fixef(model)
  #with(sound_data_balanced, tapply(att1, factor(Track:SPL:Car), mean))
}


## step function for NO MAM
.stepAllAttrNoMAM <- function(new.resp.private.sensmixed, 
                              model = model,
                              reduce.random = reduce.random, 
                              alpha.random = alpha.random, 
                              alpha.fixed = alpha.fixed, 
                              calc_post_hoc = calc_post_hoc){
  assign("new.resp.private.sensmixed", new.resp.private.sensmixed, 
         envir=environment(formula(model)))
  suppressMessages(m <- refit(object=model, newresp = new.resp.private.sensmixed, 
             rename.response = TRUE))
  suppressMessages(st <- step(m, reduce.fixed = FALSE, 
                             reduce.random = reduce.random, 
                             alpha.random = alpha.random, 
                             alpha.fixed = alpha.fixed, 
                             lsmeans.calc = TRUE,
                             difflsmeans.calc = calc_post_hoc))
  
  if(calc_post_hoc)
    st$anova.table <- .calcAvDprime(st$model, st$anova.table, 
                                    st$diffs.lsmeans.table, st$lsmeans.table)    
  st
}

## step function for MAM
.stepAllAttrMAM <- function(attr, product_structure, error_structure,
                            data, Prod_effects, random,
                            reduce.random = reduce.random, 
                            alpha.random = alpha.random, 
                            alpha.fixed = alpha.fixed, 
                            mult.scaling = mult.scaling, 
                            calc_post_hoc = calc_post_hoc){
  model.init <- suppressMessages(createLMERmodel(structure = 
                                  list(product_structure = product_structure, 
                                       error_structure = error_structure), 
                                  data = data, response = attr,
                                  fixed = list(Product = Prod_effects, 
                                             Consumer=NULL),
                                random = random, corr = FALSE, MAM = TRUE,
                                mult.scaling = mult.scaling, 
                                calc_post_hoc = calc_post_hoc))
  model.an <- model.init$model.anova
  model.lsm <- model.init$model.lsmeans
  

  st <- suppressMessages(step(model.an, fixed.calc = FALSE))
  rand.table <- st$rand.table
  
  if(reduce.random){
    anova.table <- suppressMessages(anova(as(st$model, "merModLmerTest"), 
                                          type = 1))
    if(length(which(anova.table[, "Pr(>F)"] == "NaN") > 0))
       anova.table <- suppressMessages(anova(as(st$model, "merModLmerTest"), 
                                             type = 1, ddf="Kenward-Roger")) 
  }
  else{
    anova.table <- suppressMessages(anova(model.an, type = 1))
    if(length(which(anova.table[, "Pr(>F)"] == "NaN") > 0))
       anova.table <- suppressMessages(anova(as(model.an, "merModLmerTest"), 
                                             type = 1, ddf="Kenward-Roger")) 
  }
  anova.table <- .renameScalingTerm(anova.table, Prod_effects) 
 
  if(calc_post_hoc){
    if(length(Prod_effects) > 1)
      lsmeans.table <- lsmeans::.old.lsmeans( model.lsm, pairwise ~ prod)
    else 
      lsmeans.table <-  eval(substitute(lsmeans::.old.lsmeans(object=model.lsm, 
                                                         pairwise ~ prod), 
                                        list(prod=as.name(Prod_effects)))) 
    return(list(anova.table=anova.table, rand.table=rand.table,
                lsmeans.table=lsmeans.table)) 
  }
 
  return(list(anova.table=anova.table, rand.table=rand.table)) 
}




#check if there are correlations between intercepts and slopes
checkCorr <- function(model)
{
  corr.intsl <- FALSE
  lnST <- length(getME(model, "ST"))
  for(i in 1:lnST)
  {    
    if(nrow(getME(model, "ST")[[i]])>1)
      corr.intsl <- TRUE
  } 
  return(corr.intsl) 
}