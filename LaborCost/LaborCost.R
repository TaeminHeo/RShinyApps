library(shiny)
library(ggplot2)
# Labor Cost

ui <- fluidPage(
  h1('Labor Cost'),
  mainPanel(
    fluidRow(
      column(4, align = "center",
             h3("Lower Bound (Point A)"),
             sliderInput(inputId = "lowerbound", 
                         label = "", 
                         value = 2000, min = 500, max = 3000, step = 100, width = '100%', ticks = FALSE),
             br(),
             h3("Upper Bound (Point B)"),
             sliderInput(inputId = "upperbound", 
                         label = "", 
                         value = 6000, min = 5000, max = 7500, step = 100, width = '100%', ticks = FALSE)
      ),
      column(8,
             plotOutput("FDD")
             )
    ),
    br(),
    fluidRow(
      column(4, offset = 4, align = "center",
             h3("Constuction Site#"),
             verbatimTextOutput("value"),
             h3("Actual Labor Cost"),
             verbatimTextOutput("laborcost")
      ),
      column(4,
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
    laborcost = 0, 
    trialnum = 0,
    X = array(0,dim=c(10000,1))
  )
  
  observeEvent(input$update, {
    v$trialnum <- v$trialnum + 1
  })
  
  observeEvent(input$update, {
    a <- input$lowerbound
    b <- input$upperbound
    h <- 1 / (b-a-1000)
    
    rnd <- runif(1,0,1)
    
    if (rnd <= 500*h) {
      v$laborcost = sqrt(rnd*2000/h) + a
    } else if ((500*h < rnd) & (rnd <= (b-a-1500)*h)) {
      v$laborcost = (rnd-500*h+(a+1000)*h)/h
    } else {
      v$laborcost = -sqrt(-2000*(rnd-(b-a-1000)*h)/h) + b
    }
    
    v$X[v$trialnum] = v$laborcost
  })
  
  observeEvent(input$reset, {
    v$laborcost <- 0
    v$trialnum <- 0
    v$X <- array(0,dim=c(10000,1))
  })
  
  output$FDD <- renderPlot({
    a <- input$lowerbound
    b <- input$upperbound
    h <- 1 / (b-a-1000)
    dat <- data.frame(cost = v$X[1:v$trialnum])
    pdf.X <- data.frame(cost = c(a,a+1000,b-1000,b), density = c(0,h,h,0))
    ggplot(dat, aes(x=cost)) + 
      geom_histogram(aes(y=..density..), binwidth = 500, colour="black", fill="white") + 
      geom_vline(aes(xintercept=v$laborcost, color='New Data'),linetype="longdash",size=1) +
      geom_vline(aes(xintercept=mean(cost, na.rm=T), color='Sample Mean'), linetype="longdash", size=1) +
      geom_line(data = pdf.X, aes(x=cost, y=density, color='Theoretical PDF')) +
      xlim(input$lowerbound-500, input$upperbound+500) +
      labs(title ='Frequency Density Distribution', x='Labor cost, X($/crew/week)', y='Frequency Density') +
      scale_color_manual(name = NULL, values = c('blue','red','black'))
    })
  output$value <- renderText({ v$trialnum })
  output$laborcost <- renderText({ v$laborcost })
}

shinyApp(ui = ui, server = server)
