library(shiny)
#Levee Design

ui <- fluidPage(
  h1('Levee Design'),
  br(),
  mainPanel(
    fluidRow(
      column(6, align = "center",
             h3("Levee Height (m)"),
             sliderInput(inputId = "height", 
                         label = "", 
                         value = 15, min = 0, max = 30, step = 1, width = '100%', ticks = FALSE),
             br(),
             h3("Deisgn Life (years)"),
             numericInput(inputId = "lifecycle", 
                         label = "", 
                         value = 1, min=1, max=100, width = '100%')
      ),
      column(6, align = "center",
             h3("P(Overtopping at least"),
             h3("once during Design Life)"),
             verbatimTextOutput("p_overtopping"),
             h3("Return Period"),
             verbatimTextOutput("return"),
             h3("Cost"),
             verbatimTextOutput("cost")
      )
    ),
    br(),
    fluidRow(
      column(6, offset = 6,
             h3("Clear Display", align = 'center'),
             actionButton(inputId = "reset", label = "Click", icon = icon("refresh"), width = '100%'),
             h3("Next Trial", align = 'center'),
             actionButton(inputId = "update", label = "Click", width = '100%')
      )
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(
    p_1 = 0,
    p_total = 0,
    return_period = 0, 
    cost = 0
  )
  
  observeEvent(input$update, {
    v$p_1 <- 1 - pnorm((input$height - 12)/3)
    v$return_period <- 1 / v$p_1
    v$cost <- 4500 * input$height
    v$p_total <- 1 - (1 - v$p_1)^input$lifecycle
  })
  
  observeEvent(input$reset, {
    v$p_1 <- 0 
    v$p_total <- 0 
    v$return_period <- 0 
    v$cost <- 0
  })
  
  output$p_overtopping <- renderText({ 
    round(v$p_total,4)
  })
  output$return <- renderText({ 
    round(v$return_period,2)
  })
  output$cost <- renderText({ 
    v$cost 
  })
}

shinyApp(ui = ui, server = server)