# server.R
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
    source('runAnalysis.R', local=TRUE)
    
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
  source('stepUtils.R', local=TRUE)
  source('posthocUtils.R', local=TRUE)
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
   

  ## here the server part of the UI is sourced
  source('serverUI.R', local = TRUE)

  
  addTooltip(session, "plotsSensMixed", "title", placement = "bottom", 
             trigger = "click") 
  session$onSessionEnded(function() { stopApp() })

})