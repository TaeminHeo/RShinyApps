library(shiny)
#Landfill Perfromance

ui <- fluidPage(
  h1('Landfill Performance'),
  br(),
  mainPanel(
    fluidRow(
      column(6, align = "center",
             h3("Liner's Performance"),
             verbatimTextOutput("Leakage"),
             br(),
             h3("Monitoring Well's Performance"),
             verbatimTextOutput("Monitoring")
      ),
      
      column(6, align = "center",
             h3("Expected Cleanup Cost ($millions)"),
             verbatimTextOutput("totalcost")
      )
    ),
    br(),
    fluidRow(
      column(8, align = "center",
             imageOutput("image")
      ),
      column(4,
             h3("Clear Display", align = 'center'),
             actionButton(inputId = "reset", label = "Click", icon = icon("refresh"), width = '100%'),
             h3("Trial", align = 'center'),
             verbatimTextOutput("value"),
             h3("Next Trial", align = 'center'),
             actionButton(inputId = "update", label = "Click", width = '100%')
      )
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(
    num_labtest = NULL, 
    num_leakage = NULL, 
    statusM = c("Does NOT Detect Leakage"),
    statusL = c("Has NOT Leaked"),
    totalcost = 0,
    trialnum = 0,
    img = 'L0M0.png'
  )
  
  observeEvent(input$update, {
    v$num_labtest <- rnorm(1,mean = 0, sd = 1)
  })
  observeEvent(input$update, {
    v$num_leakage <- rnorm(1,mean = 0, sd = 1)
  })
  
  observeEvent(input$update, {
    if(v$num_leakage < qnorm(0.15)) {
      v$statusL <- c("Has Leaked")
      if(v$num_labtest < qnorm(0.25)) {
        v$statusM <- c("Does NOT Detect Leakage")
        v$totalcost <- 210
        v$img <- 'LM0.png'
      } else {
        v$statusM <- c("Detects Leakage")
        v$totalcost <- 60
        v$img <- 'LM.png'
      }
    } else {
      v$statusL <- c("Has NOT Leaked")
      if(v$num_labtest < qnorm(0.25)) {
        v$statusM <- c("Detects Leakage")
        v$totalcost <- 60
        v$img <- 'L0M.png'
      } else {
        v$statusM <- c("Does NOT Detect Leakage")
        v$totalcost <- 0
        v$img <- 'L0M0.png'
      }
    }
  })
  
  observeEvent(input$update, {
    v$trialnum <- v$trialnum + 1
  })
  
  observeEvent(input$reset, {
    v$num_monitor <- NULL 
    v$num_leakage <- NULL 
    v$statusM <- c("Does NOT Detect Leakage")
    v$statusL <- c("Has NOT Leaked")
    v$trialnum <- 0
    v$totalcost <- 0
  })
  
  output$Monitoring <- renderText({ 
    v$statusM
  })
  output$Leakage <- renderText({ 
    v$statusL
  })
  output$value <- renderText({ 
    v$trialnum 
  })
  output$totalcost <- renderText({ 
    v$totalcost 
  })
  output$image <- renderImage({
    #w  <- session$clientData$output_image_width
    #h <- session$clientData$output_image_height
    w <- 500
    h <- 250
    
    return(list(
      src = v$img,
      contentType = 'image/png',
      width = w,
      height = h,
      alt = "IMG"))
    
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)