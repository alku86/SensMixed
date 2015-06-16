if(input$analysis == "Consumer data"){
  df.raw <- convertToFactors(df.raw, c(input$Consumers, input$Products,
                                       input$Consumerfact))
  withProgress( message = "Calculating, please wait",
                detail = "This may take a few moments...",{ 
                  res <- tryCatch({consmixed(response = input$Response, 
                                             Prod_effects= input$Products, 
                                             Cons_effects = input$Consumerchar, 
                                             Cons = input$Consumers, data =ham, 
                                             structure = input$struct, 
                                             alpha.random = as.numeric(input$alpharand),
                                             alpha.fixed = as.numeric(input$alphafixed))}, 
                                  error = function(e) { NULL })     
                })
}else{
  df.raw <- convertToFactors(df.raw, c(input$Assessors, input$Products, 
                                       input$Replications))
  
  
  withProgress(message = "Calculating, please wait",
               detail = "This may take a few moments...", {
                 
                 
                 res <- tryCatch({sensmixed(input$Attributes,
                                            Prod_effects=input$Products, 
                                            replication = input$Replications,
                                            individual=input$Assessors, 
                                            data=df.raw, 
                                            calc_post_hoc = as.logical(input$calc_post_hoc), 
                                            product_structure = as.numeric(input$struct),
                                            error_structure = input$errstruct,
                                            alpha.random = as.numeric(input$alpharand),
                                            alpha.fixed = as.numeric(input$alphafixed),
                                            reduce.random = as.logical(input$simplerr),
                                            MAM = as.logical(input$MAM),
                                            keep.effs = unlist(strsplit(input$keep, " ")),
                                            parallel = FALSE, 
                                            mult.scaling = as.logical(input$multMAM),
                                            oneway_rand = as.logical(input$oneway_rand))}, 
                                 error = function(e) { NULL })
               })
}