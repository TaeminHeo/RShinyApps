library(shiny)
#Levee Simulation

ui <- fluidPage(
  h1('Levee Simulation'),
  br(),
  mainPanel(
    fluidRow(
      column(5, align = "center",offset = 1,
             h3("Levee Height (m)"),
             sliderInput(inputId = "height", 
                         label = "", 
                         value = 15, min = 0, max = 30, step = 1, width = '100%', ticks = FALSE)
      ),
      column(5, align = "center", offset = 1,
             h3("Design Life (years)"),
             numericInput(inputId = "lifecycle", 
                          label = "", 
                          value = 20, min=1, max=100, width = '100%')
      )
    ),
    br(),
    h3("Levee Performance", align = "center"),
    fluidRow(
      column(2, align = "center", offset = 1,
             h3("Year 1"),
             htmlOutput("performance1"),
             tags$p(tags$style("#performance1{border:1px; border-style:solid; padding: 0.5em;}"))
      ),
      column(2, align = "center",
             h3("Year 2"),
             htmlOutput("performance2"),
             tags$p(tags$style("#performance2{border:1px; border-style:solid; padding: 0.5em;}"))
      ),
      column(2, align = "center",
             h3("Year 3"),
             htmlOutput("performance3"),
             tags$p(tags$style("#performance3{border:1px; border-style:solid; padding: 0.5em;}"))
      ),
      column(2, align = "center",
             h3("Year 4"),
             htmlOutput("performance4"),
             tags$p(tags$style("#performance4{border:1px; border-style:solid; padding: 0.5em;}"))
      ),
      column(2, align = "center",
             h3("Year 5"),
             htmlOutput("performance5"),
             tags$p(tags$style("#performance5{border:1px; border-style:solid; padding: 0.5em;}"))
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
    water1 = NULL,
    water2 = NULL,
    water3 = NULL,
    water4 = NULL,
    water5 = NULL,
    performance1 = "Intact",
    performance2 = "Intact",
    performance3 = "Intact",
    performance4 = "Intact",
    performance5 = "Intact",
    trialnum = 0
  )
  
  observeEvent(input$update, {
    
    v$performance1 <- "Intact"
    v$performance2 <- "Intact"
    v$performance3 <- "Intact"
    v$performance4 <- "Intact"
    v$performance5 <- "Intact"
    
    v$water1 <- rnorm(1,12,3)
    v$water2 <- rnorm(1,12,3)
    v$water3 <- rnorm(1,12,3)
    v$water4 <- rnorm(1,12,3)
    v$water5 <- rnorm(1,12,3)
    
    if (v$water1 > input$height){ v$performance1 <- "Failed" }
    if (v$water2 > input$height){ v$performance2 <- "Failed" }
    if (v$water3 > input$height){ v$performance3 <- "Failed" }
    if (v$water4 > input$height){ v$performance4 <- "Failed" }
    if (v$water5 > input$height){ v$performance5 <- "Failed" }
      
  })
  
  observeEvent(input$update, {
    v$trialnum <- v$trialnum + 1
  })
  
  observeEvent(input$reset, {
    v$water1 <- NULL
    v$water2 <- NULL
    v$water3 <- NULL
    v$water4 <- NULL
    v$water5 <- NULL
    v$performance1 <- "Intact"
    v$performance2 <- "Intact"
    v$performance3 <- "Intact"
    v$performance4 <- "Intact"
    v$performance5 <- "Intact"
    v$trialnum <- 0
  })
  
  output$performance1 <- renderText({ 
    if (v$performance1=="Intact"){
      paste("<div class='boxed'><font color=\"#3CCE5C\"><b>",v$performance1,"</div>")
      }
    else{
      paste("<font color=\"#FF0000\"><b>",v$performance1)
      }
   
  })
  output$performance2 <- renderText({ 
    if (v$performance2=="Intact"){  
      paste("<div class='boxed'><font color=\"#3CCE5C\"><b>",v$performance2,"</div>")
  }
  else{
    paste("<font color=\"#FF0000\"><b>",v$performance2)
  }
  })
  output$performance3 <- renderText({ 
    if (v$performance3=="Intact"){  
      paste("<div class='boxed'><font color=\"#3CCE5C\"><b>",v$performance3,"</div>")
    }
    else{
      paste("<font color=\"#FF0000\"><b>",v$performance3)
    }
  })
  output$performance4 <- renderText({ 
    if (v$performance4=="Intact"){  
      paste("<div class='boxed'><font color=\"#3CCE5C\"><b>",v$performance4,"</div>")
    }
    else{
      paste("<font color=\"#FF0000\"><b>",v$performance4)
    }
  })
  output$performance5 <- renderText({ 
    if (v$performance5=="Intact"){  
      paste("<div class='boxed'><font color=\"#3CCE5C\"><b>",v$performance5,"</div>")
    }
    else{
      paste("<font color=\"#FF0000\"><b>",v$performance5)
    }
  })
  output$value <- renderText({ 
    v$trialnum 
  })
  output$image <- renderImage({
    
    return(list(
      src = 'img.png',
      contentType = 'image/png',
      width = 600,
      height = 200,
      alt = "IMG"))
    
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)