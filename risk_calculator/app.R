  library(shiny)
  library(shinyjs)
  
  PAGE_TITLE <- "calculator"
  # Define UI for application
  ui <- fluidPage(
    #header image
    titlePanel(windowTitle = PAGE_TITLE,
               title =
                 tags$a(href='https://biostat.korea.ac.kr/',
                        tags$img(src='https://github.com/James1verse27/risk_calculator/blob/main/risk_calculator/www/korea_biostat_logo.png?raw=true',width = "20%", height = "20%" ))),
    
    h6(div(HTML("<em>\nSelect the model parameters below to calculate risk estimate:\n\n</em>"))),
    
    useShinyjs(),
    #create client side input form
    sidebarLayout(
      sidebarPanel(
                   div(
                     id="form",
                     sliderInput(inputId = 'pelvic_LNR', label = 'pelvic_LNR', min=0, max=0.9,value=0.05, step=0.01, round=0),
                     selectInput(inputId = "SCC_Ag",label = "SCC_Ag", choices = c('1','0'), selected = "1"),
                     selectInput(inputId = "size",label = "size", choices = c('1','0'), selected = "1"),
                     selectInput(inputId = "path",label = "path", choices = c('1','2','3','4'), selected = '1'),
                     actionButton("calc", "Calculate"),
                     verbatimTextOutput("final_val"),
                     
                   )),
      mainPanel(div(img(src="https://github.com/James1verse27/risk_calculator/blob/main/risk_calculator/www/nomogram.png?raw=true",height = 700), style="text-align: center;"))),
    
    #footer
    hr(),
    tags$a(href='https://github.com/James1verse27/risk_calculator/',
           tags$img(src='https://github.com/James1verse27/risk_calculator/blob/main/risk_calculator/www/github_logo.jpg?raw=true',width = "5%", height = "5%" )),
    tags$a(href='https://james1verse27.github.io/',
           tags$img(src='https://github.com/James1verse27/James1verse27.github.io/blob/main/assets/icons/mstile-150x150.png?raw=true',width = "5%", height = "5%" )),
    tags$a(href="mailto:kjh8661@korea.ac.kr%20",
           tags$img(src='https://github.com/James1verse27/risk_calculator/blob/main/risk_calculator/www/gmail_logo.png?raw=true',width = "2.5%", height = "2.5%" , align="middle"))
  )
  
  
  
  # Define server logic
  server <- function(input, output) {
    paramValues <- reactive({
      
      # Compose data frame
      data.frame(
        Parameter = c("pelvic_LNR","SCC_Ag","size","path"),
        Selection = c(input$pelvic_LNR,
                      input$SCC_Ag,
                      input$size,
                      input$path),
        stringsAsFactors=FALSE)
    })
    
    #estimate risk score based on input model parameters:
    calculate <- function(pelvic_LNR, SCC_Ag, size, path) {
      est.30days=0.863
      B=0.14946
      beta_xbar= 0.3005163
      if (pelvic_LNR<0.05){
        pelvic_LNR_Points=0
      }
      else if(pelvic_LNR>=0.05 & pelvic_LNR<0.2){
        pelvic_LNR_Points=3
      }
      else{
        pelvic_LNR_Points=7
      }
      if(SCC_Ag=="0"){
        SCC_Ag_Points=0
      }
      else{
        SCC_Ag_Points=-2
      }
      if(size=="0"){
        size_Points=0
      }
      else{
        size_Points=3
      }
      if(path==1){
        path_Points=0
      }
      else if(path==2){
        path_Points=5
      }
      else if(path==3){
        path_Points=2
      }
      else{
        path_Points=4
      }
      #calculate total points
      totalPoints = (pelvic_LNR_Points + SCC_Ag_Points + size_Points + path_Points)
      risk.score=B*totalPoints-beta_xbar
      risk = 1-(est.30days^(exp(risk.score)))
      return(risk)
    }
    
    output$final_tbl <- renderTable({
      if (input$calc == 0)
        return()
      isolate(paramValues())
    })
    
    output$final_val <- renderText({
      if (input$calc == 0){
        return()
      }
      else{
        isolate(paste("Estimated Risk: ", round(calculate(input$pelvic_LNR, 
                                                          input$SCC_Ag,
                                                          input$size,
                                                          input$path),digits = 4)))
      }
    })
    
  }
  # Run the application
  shinyApp(ui = ui, server = server)
