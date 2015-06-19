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
                    selectizeInput("Attributes", "Select attributes", names.dd,
                                   options = list(dropdownParent = 'body'),
                                   multiple = TRUE),           
                    selectInput("Assessors", "Select assessor", 
                                names.dd),
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