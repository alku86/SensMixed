# server.R

library(SensMixed)
library(shiny)
library(googleVis)
library(shinyBS)
library(ggplot2)
library(Hmisc)
library(xtable)


shinyServer(function(input, output, session) {
  tags$style(type="text/css", ".tab-content { overflow: visible; }")
  tags$head(
    tags$style(type="text/css", "html {overflow:hidden;}"))
  uploadData <- reactive({
    if(input$uploaddata == 1){
      inFile <- input$file1
      
      if (is.null(inFile))
        {return()}
      
      
      return(read.delim(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote,
                        dec = input$decimal, fileEncoding="UTF-8-BOM"))
    }
    else if(input$uploaddata == 2)
      return(TVbo)
    else 
      return(ham)   
  })
  
  Data <- reactive({    
     input$goButton
  isolate({
     if (is.null(uploadData()))
       {return()}
    
    df.raw <- uploadData()     
    
    ## here the analysis of consumer/sensory data is sourced
    ## and saved in res variable
    #source('runAnalysis.R', local=TRUE)
    if(input$analysis == "Consumer data"){
      df.raw <- convertToFactors(df.raw, c(input$Consumers, input$Products,
                                           input$Consumerfact))
      withProgress( message = "Calculating, please wait",
                    detail = "This may take a few moments...",{ 
                      res <- tryCatch({consmixed(response = input$Response, 
                                                 Prod_effects = input$Products, 
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
                     
                     res <- tryCatch({sensmixed(names(df.raw)[5:ncol(df.raw)],
                                                Prod_effects=c("TVset", "Picture"), 
                                                individual="Assessor", 
                                                data=df.raw, 
                                                calc_post_hoc = TRUE, 
                                                product_structure = 3,
                                                error_structure = "3-WAY",
                                                alpha.random = 0.1,
                                                alpha.fixed = 0.05,
                                                reduce.random = TRUE,
                                                MAM = FALSE,
                                                keep.effs = NULL,
                                                parallel = FALSE, 
                                                mult.scaling = FALSE,
                                                oneway_rand = FALSE)}, 
                                     error = function(e) { NULL })
                   })
    }    
    return(res)
  })
  })
  
  ##### call utils functions ########################################################
  sensmixedPlot <- function(){
    if(input$analysis == "Consumer data") { return() }
    
    
    if(input$typeEffs == 1)
      return(plot(Data(), mult = input$representPlot, isFixed = FALSE, 
                  isScaling = FALSE, cex = 2))
    if(input$typeEffs == 2)
      return(plot(Data(), mult = input$representPlot, isRand = FALSE, 
                  isScaling = FALSE, 
                  dprime = input$typePlot, cex = 2))
    if(input$typeEffs == 3)
      return(plot(Data(), mult = input$representPlot, isRand = FALSE, 
                  isFixed = FALSE, cex = 2))    
  } 
  
  ## here the step results are formatted using xtable
  #source('stepUtils.R', local=TRUE)
  ## TODO: partition the code
  
  getNameStep <- function(){
    if(input$analysis == "Consumer data")
      return("stepoutput")
    else
      return(input$AttrStep)
  }
  
  
  ## return table of results for the step function for the random part
  ## in latex or html formats using xtable
  ## for sensory/consumer data
  stepRandResult <- function(){
    if (is.null(Data())) {return()} 
    if(input$analysis == "Consumer data"){
      rnd <- Data()$rand.table
      rnd[ , "p.value"] <- 
        format.pval(rnd[, "p.value"], digits=3, eps=1e-3)
      rnd_tab <- xtable(rnd, align = 
                          paste(c("l", rep("c", ncol(rnd))), collapse = ""), 
                        display= c("s","f","d","s","s"))
      caption(rnd_tab) <- paste("Likelihood ratio tests for the 
                                random-effects and their order of elimination representing Step 1 of 
                                the automated analysis") 
      
      
      
      print(rnd_tab, caption.placement="top", table.placement="H", 
            type = input$typetable,
            html.table.attributes = getOption("xtable.html.table.attributes",
                                              "rules='groups' width='100%'"))
      
    }
    else{
      if(is.null(input$AttrStep) || length(input$AttrStep)>1)
      {return()}
      st <- Data()$step_res[[input$AttrStep]] 
      
      st$rand.table[ , "p.value"] <- format.pval(st$rand.table[, "p.value"], 
                                                 digits=3, eps=1e-3)
      if(ncol(st$rand.table) == 3){
        colnames(st$rand.table) <- c("Chi.sq","Chi.DF" , "p-value")
        rand.table_tv <- xtable(st$rand.table, align="lccc", 
                                display=c("s","f","d","s"))
      }
      else{
        colnames(st$rand.table) <- c("Chi.sq","Chi.DF" , "elim.num", "p-value")
        rand.table_tv <- xtable(st$rand.table, align="lcccc", 
                                display=c("s","f","d","d","s"))
      }     
      caption(rand.table_tv) <- paste("Likelihood ratio tests for the 
                                      random-effects and their order of elimination representing Step 1 of 
                                      the automated analysis for the attribute", input$AttrStep)       
      
      print(rand.table_tv, caption.placement="top", table.placement="H", 
            type = input$typetable, 
            html.table.attributes = getOption("xtable.html.table.attributes",
                                              "rules='groups' width='100%'")) 
      
    }    
  }
  
  
  ## return table of results for the step function for the fixed part
  ## in latex or html formats using xtable
  ## for sensory/consumer data
  stepFixedResult <- function(){
    if (is.null(Data())) {return()}
    
    if(input$analysis == "Consumer data"){
      an <- Data()$anova.table
      an[, "Pr(>F)"] <- format.pval(an[, "Pr(>F)"], digits=3, eps=1e-3)
      if("elim.num" %in% colnames(an))
        an_tab <- xtable(an, align = paste(c("l", rep("c", ncol(an))), 
                                           collapse = ""), 
                         display = c("s","f","f","d","f","f","s", "s"))
      else
        an_tab <- xtable(an, align = paste(c("l", rep("c", ncol(an))), 
                                           collapse = ""), 
                         display = c("s","f","f","d","f","f", "s"))
      caption(an_tab) <- 
        paste("F-tests for the fixed-effects and their order of elimination representing Step 3 of the automated analysis")
      
      print(an_tab, caption.placement="top",
            table.placement="H", 
            type = input$typetable, 
            html.table.attributes = 
              getOption("xtable.html.table.attributes",
                        "rules='groups' width='100%'"))    
      
    }
    else{
      if(is.null(input$AttrStep) || length(input$AttrStep)>1)
      {return()}
      
      
      
      st <- Data()$step_res[[input$AttrStep]] 
      
      
      st$anova.table[, "Pr(>F)"] <- format.pval(st$anova.table[, "Pr(>F)"], 
                                                digits=3, eps=1e-3)
      if("dprimeav" %in% colnames(st$anova.table)){
        colnames(st$anova.table) <-
          c("Sum Sq", "Mean Sq", "NumDF", "DenDF", "F-value","d-prime", "Pr(>F)")
        anova.table_tv <- xtable(st$anova.table, align="lccccccc", 
                                 display=c("s","f","f","s","f","f","f", "s"))
      }else{
        colnames(st$anova.table) <-
          c("Sum Sq", "Mean Sq", "NumDF", "DenDF", "F-value", "Pr(>F)")
        anova.table_tv <- xtable(st$anova.table, align="lcccccc", 
                                 display=c("s","f","f","s","f","f", "s"))
      }      
      
      caption(anova.table_tv) <- 
        paste("F-tests for the fixed-effects  for the attribute",
              input$AttrStep)
      
      print(anova.table_tv, caption.placement="top",
            table.placement="H", 
            type = input$typetable,
            html.table.attributes = 
              getOption("xtable.html.table.attributes",
                        "rules='groups' width='100%'"))     
    }  
    
  }
  #source('posthocUtils.R', local=TRUE)
  posthocResult <- function(){
    if (is.null(Data())) {return()}
    if(input$analysis == "Consumer data"){
      result <- Data()
      names.lsm <- "Population means for attribute"
      names.dlsm <- "Multiple comparison tests"
    }
    else{
      if(is.null(input$AttrPosthoc) || length(input$AttrPosthoc)>1)
      {return()}
      if(!("post_hoc" %in% names(Data()))) {return()}
      
      result <- Data()$step_res[[input$AttrPosthoc]]     
      
      names.lsm <- paste("Population means for attribute ", 
                         input$AttrPosthoc)
      names.dlsm <- paste("Multiple comparison tests for attribute ",
                          input$AttrPosthoc)    
    } 
    
    if(input$whichPlot == "LSMEANS"){
      ph <- result$lsmeans.table
      
      rnames <- rownames(ph)
      diffs.facs <- sapply(rnames, 
                           function(x) 
                             substring(x, 1, 
                                       substring.location(x, " ")$first[1]-1), 
                           USE.NAMES = FALSE)    
      find.fac <- diffs.facs %in% input$effsPlot
      ph <- ph[find.fac,]
      ph[, which(colnames(ph)=="p-value")] <- 
        format.pval(ph[, which(colnames(ph)=="p-value")], digits=3, eps=1e-3)
      ph_tab <- xtable(ph, align = paste(c("l", rep("c", ncol(ph))), 
                                         collapse = ""), 
                       display = c(rep("s",
                                       which(colnames(ph) == "Estimate")), 
                                   rep("f", 6), "s"))
      
      caption(ph_tab) <- names.lsm
      print(ph_tab, caption.placement="top",
            table.placement="H", 
            type = "html",
            html.table.attributes = getOption("xtable.html.table.attributes",
                                              "rules='groups' width='105%'"))
      
    }
    else{
      ph <- result$diffs.lsmeans.table
      rnames <- rownames(ph)
      diffs.facs <- sapply(rnames, 
                           function(x) 
                             substring(x, 1, 
                                       substring.location(x, " ")$first[1]-1), 
                           USE.NAMES = FALSE)    
      find.fac <- diffs.facs %in% input$effsPlot
      ph <- ph[find.fac,]
      
      ph[, 7] <- format.pval(ph[, 7], digits=3, eps=1e-3)
      
      ph_tab <- xtable(ph, align="lccccccc", 
                       display=c("s","f","f","f","f","f","f", "s"))
      caption(ph_tab) <- names.dlsm
      
      print(ph_tab, caption.placement="top",
            table.placement="H", 
            type = "html",
            html.table.attributes = 
              getOption("xtable.html.table.attributes",
                        "rules='groups' width='105%'"))
      
    }
  }
  
  posthocPlot <- function(){
    if (is.null(Data())) {return()}
    if(input$analysis == "Consumer data")
      plot(Data(), cex = 1.6, 
           which.plot = input$whichPlot, effs = input$effsPlot) 
    else{
      if(!("post_hoc" %in% names(Data()))) {return()}
      
      if(is.null(input$AttrPosthoc) || length(input$AttrPosthoc)>1)
      {return()}
      
      if(input$MAM == "TRUE"){
        if(input$whichPlot == "LSMEANS")
          tab <- Data()$step_res[[input$AttrPosthoc]]$lsmeans.table
        else
          tab <- Data()$step_res[[input$AttrPosthoc]]$diffs.lsmeans.table
        plotLSMEANS(table = tab, 
                    response = Data()$step_res[[input$AttrPosthoc]]$response, 
                    which.plot = input$whichPlot, effs = input$effsPlot)
      }
      else
        plot(Data()$step_res[[input$AttrPosthoc]], cex = 1.6, 
             which.plot = input$whichPlot, effs = input$effsPlot) 
    }       
  }
  ##############################################################################

  
  output$plotsSensMixed <- renderPlot({   
    sensmixedPlot()  
  })

  output$downloadPlot <- downloadHandler(
    filename = function() { paste("plotSensmixed",input$typeEffs, 
                                  '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, 
                                                            height = height, 
                                                            res = 300, 
                                                            units = "in")
      ggsave(file, sensmixedPlot(), scale = input$scalePlot, device = device)
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function() { paste("tableSensmixed", input$typeEffsTable, 
                                  '.doc', sep='') },
    content = function(file) {
      sink(file)
      saveToDoc(Data(), type = input$typetable2, typeEffs = input$typeEffsTable)
      sink()
    }, contentType = 'text/plain'
  )
  
  output$downloadStep <- downloadHandler(
    filename = function() { paste(getNameStep(), '.doc', sep='') },
    content = function(file) {
      sink(file)
      stepRandResult() 
      stepFixedResult()
      sink()
    }, contentType = 'text/plain'
  )
  
  output$downloadPosthocTable <- downloadHandler(
    filename = function() { paste(input$AttrPosthoc, input$whichPlot, 
                                  input$effsPlot, '.doc', sep='') },
    content = function(file) {
      sink(file)
      posthocResult()
      sink()
    }, contentType = 'text/plain'
  )
  
  output$downloadPosthocPlot <- downloadHandler(
    filename = function() { paste(input$AttrPosthoc, input$whichPlot, 
                                  input$effsPlot, '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, 
                                                            height = height, 
                                                            res = 300, 
                                                            units = "in")
      ggsave(file, posthocPlot(), device = device)
    }
  )

  
  output$tablesSensMixed <- renderPrint({
    if(is.null(uploadData())) { return() }
    if(input$analysis == "Consumer data") { return() }
    if(is.null(Data())){return()}
    saveToDoc(Data(), type = input$typetable2, typeEffs = input$typeEffsTable)    
  })

  output$stepRand <- renderPrint({
    stepRandResult()  
  })

  output$stepFixed <- renderPrint({
    stepFixedResult()
  })

  output$posthocTable <- renderPrint({
    posthocResult()
  })

  output$posthocPlot <- renderPlot({
    posthocPlot()
  })
   
  output$contents <- renderDataTable({
    if(!is.null(uploadData()))
      return(uploadData())
    
  })

  output$helpprodstruct <- renderTable({
    helpprodstruct <- matrix(NA, nrow = 3, ncol = 1)
    rownames(helpprodstruct) <- c(1,2,3)
    colnames(helpprodstruct) <- "Explanations"
    helpprodstruct[1,1] <- "only main effects will enter the initial model"
    helpprodstruct[2,1] <- "main effects and 2-way interaction"
    helpprodstruct[3,1] <- "all main effects and all possible interaction"
    return(xtable(helpprodstruct))
  })

  output$helperrstruct <- renderTable({
    helperrstruct  <- matrix(NA, nrow = 3, ncol = 1)
    rownames(helperrstruct) <- c("No-Rep","2-WAY","3-WAY")
    colnames(helperrstruct) <- "Explanations"
    helperrstruct[1,1] <- "assessor effect and all possible interactions between assessor and product effects"
    helperrstruct[2,1] <- "No-Rep + replicate effect and replicate assessor interaction effect"
    helperrstruct[3,1] <- "assessor and replicate effect and interaction between them and interaction between them and Product_effects"
    return(xtable(helperrstruct))
  })
  
  output$helponeway <- renderTable({
    helponeway  <- matrix(NA, nrow = 2, ncol = 1)
    rownames(helponeway) <- c("No", "Yes")
    colnames(helponeway) <- "Explanations"
    helponeway[1,1] <- "considers multi-way product structure in the random part"
    helponeway[2,1] <- "considers just one product factor in the random part, where the product factor is chosen as the overall product factor combining each product-combination into a single factor with as many levels as there are different product combinations"    
    return(xtable(helponeway))
  })
   

  ## here the server part of the UI is sourced
  #source('serverUI.R', local = TRUE)
  #### The file contains the server part of the UI (interactive UI)
  
  ## constructs tab panel for the input controls
  tabPanel.input <- function(names.dd){
    if(input$analysis == "Consumer data")
      return(tabPanel("Input arguments",
                      selectInput("Response", "Select response", names.dd),           
                      selectInput("Consumers", "Select consumer", names.dd),
                      selectizeInput("Products", "Select products", names.dd,  
                                     options = list(dropdownParent = 'body'),
                                     multiple = TRUE),
                      selectizeInput("Consumerchar", 
                                     "Select consumer characteristics", 
                                     names.dd,  
                                     options = list(dropdownParent = 'body'),
                                     multiple = TRUE),
                      selectizeInput("Consumerfact", 
                                     "Consumer characteristics treated as factors", 
                                     names.dd,  
                                     options = list(dropdownParent = 'body'),
                                     multiple = TRUE)
      ))
    else
      return(tabPanel("Input arguments",
                      selectizeInput("Attributes", "Select attributes", 
                                     names.dd,
                                     options = list(dropdownParent = 'body'),
                                     selected = names.dd[5:length(names.dd)],
                                     multiple = TRUE),           
                      selectInput("Assessors", "Select assessor", 
                                  names.dd, selected = 
                                    names.dd[1]),
                      selectInput("Replications", "Select replications", 
                                  names.dd),
                      selectizeInput("Products", "Select products", names.dd,  
                                     options = list(dropdownParent = 'body'),
                                     multiple = TRUE)
      ))
  }
  
  ## constructs tab panel for the modelling controls
  tabPanel.model <- function(){
    if(input$analysis == "Consumer data")
      return(tabPanel("Modelling controls",
                      selectInput('struct', 'Select structure', 
                                  c("1" = 1, "2" = 2, "3" = 3))             
      ))
    else
      return(tabPanel("Modelling controls",                
                      selectInput('struct', 
                                  'Select product structure', 
                                  c("1" = 1, "2" = 2, "3" = 3)),
                      bsCollapsePanel("Help product structure", 
                                      tableOutput("helpprodstruct"), id="col1", 
                                      value="test1"),
                      selectInput('errstruct', 
                                  'Select error structure', 
                                  c("No_Rep" = "No_Rep", 
                                    "2-WAY" = "2-WAY", 
                                    "3-WAY" = "3-WAY")),
                      bsCollapsePanel("Help error structure", 
                                      tableOutput("helperrstruct"), 
                                      id="col2", value="test2"),
                      selectInput('oneway_rand', 'One-way product random part', 
                                  c( "No" = FALSE, "Yes" = TRUE)),
                      bsCollapsePanel("Help one-way product random part", 
                                      tableOutput("helponeway"), 
                                      id="col2", value="test2"),
                      selectInput('MAM', 'Correct for scaling', c("Yes" = TRUE, 
                                                                  "No" = FALSE)),
                      selectInput('multMAM', 'Mult-way scaling', c("No" = FALSE, 
                                                                   "Yes" = TRUE))                  
      ))
  }
  
  ## constructs tab panel for the analysis controls
  tabPanel.an <- function(){
    if(input$analysis == "Consumer data")
      return(tabPanel("Analysis controls",
                      selectInput('alpharand', 
                                  'Type 1 error for testing random effects', 
                                  c("0.1" = 0.1, "0.2" = 0.2, "0.05" = 0.05)),
                      selectInput('alphafixed', 
                                  'Type 1 error for testing fixed effects', 
                                  c("0.05" = 0.05, "0.01" = 0.01, 
                                    "0.001" = 0.001)))
      )
    else
      return(tabPanel("Analysis controls",                     
                      selectInput('calc_post_hoc', 'Calculate post-hoc', 
                                  c("Yes" = TRUE, "No" = FALSE)),
                      selectInput('simplerr', 'Simplification of error structure', 
                                  c("Yes" = TRUE, "No" = FALSE)),
                      textInput("keep", label = "Effects to keep in a model", 
                                value = "Enter effects separated by space..."),
                      selectInput('alpharand', 
                                  'Type 1 error for testing random effects', 
                                  c("0.1" = 0.1, "0.2" = 0.2, "0.05" = 0.05)),
                      selectInput('alphafixed', 
                                  'Type 1 error for testing fixed effects', 
                                  c("0.05" = 0.05, "0.01" = 0.01, 
                                    "0.001" = 0.001))
      ))
  }
  
  output$AttrUI <- renderUI({ 
    if(is.null(uploadData()))
    {names.dd <- NULL}
    else{
      dd <- uploadData()
      names.dd <- colnames(dd)
    }
    tabsetPanel(
      tabPanel.input(names.dd),
      tabPanel.model(),
      tabPanel.an()
    )  
  })
  
  
  
  output$AttrStepUI <- renderUI({
    if(is.null(Data())) {return()}    
    if(input$analysis == "Consumer data") {
      list(
        selectInput("typetable", "Type", c("html", "latex")),
        downloadButton('downloadStep', label = "Download Table")
      )
    } 
    else{
      list(
        selectInput("AttrStep", "Select attribute", names(Data()$step_res)),
        selectInput("typetable", "Type", c("html", "latex")),
        downloadButton('downloadStep', label = "Download Table"))
    }
  })
  
  output$AttrPosthocUI <- renderUI({
    if(is.null(Data()))    {return()}   
    if(input$analysis == "Consumer data") {
      selectInput("whichPlot", "Type of Plot", 
                  c("LSMEANS" = "LSMEANS", 
                    "DIFF of LSMEANS" = "DIFF of LSMEANS"))
    }
    else{
      list(
        selectInput("AttrPosthoc", "Select attribute", names(Data()$step_res)),    
        selectInput("whichPlot", "Type of Plot", 
                    c("LSMEANS" = "LSMEANS", 
                      "DIFF of LSMEANS" = "DIFF of LSMEANS")))      
    }
    
    
  })
  
  output$EffsPosthocUI <- renderUI({
    if(is.null(Data()))    {return()}   
    if(input$analysis == "Consumer data"){
      an.table <- Data()$anova.table
    }
    else{
      if(is.null(input$AttrPosthoc) || length(input$AttrPosthoc)>1)
      {return()}
      an.table <- Data()$step_res[[input$AttrPosthoc]]$anova.table
    }    
    
    if("elim.num" %in% colnames(an.table)){
      effs <- rownames(an.table[which(an.table$elim.num == "kept"), , 
                                drop = FALSE])
    }
    else
      effs <- rownames(an.table)
    list(
      selectInput("effsPlot", "Effects", effs),
      downloadButton('downloadPosthocTable', label = "Download Table"),
      downloadButton('downloadPosthocPlot', label = "Download Plot")
    )
  })
  
  output$UploadUI <- renderUI({    
    if(input$uploaddata == 1){ 
      verticalLayout(
        
        #tags$hr(),
        fileInput('file1', 
                  'Choose CSV File from local drive, adjusting parameters if necessary',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Semicolon=';',
                       Comma=',',
                       Tab='\t'),
                     'Semicolon'),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     'Double Quote'),
        radioButtons('decimal', 'Decimal',
                     c("Period" = ".", "Comma" = ",")),
        tags$head(tags$style(type="text/css",
                             "label.radio { display: inline-block; margin:0 10 0 0;  }",
                             ".radio input[type=\"radio\"] { float: none; }")),
        mainPanel(
          dataTableOutput('contents')
        )      
        
      )
    }
    else{
      verticalLayout(        
        mainPanel(
          dataTableOutput('contents')
        )         
      )
    }   
    
  })

  
  addTooltip(session, "plotsSensMixed", "title", placement = "bottom", 
             trigger = "click") 
  session$onSessionEnded(function() { stopApp() })

})