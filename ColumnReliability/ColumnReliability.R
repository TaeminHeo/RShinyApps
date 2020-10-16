library(shiny)
library(ggplot2)
# Functions of Random Variables

ui <- fluidPage(
  h1('Column Reliability'),
  sidebarLayout(
    sidebarPanel(
    h3("Input Value", align = "center"),
    fluidRow( 
             h4("Mean Capacity (N)"),
             sliderInput(inputId = "mean_C",
                         label = "", 
                         value = 5300, min = 0, max = 6000, step = 100, width = '100%', ticks = FALSE)
      ),
    fluidRow(
             h4("Standard Deviation of Capacity (N)"),
             sliderInput(inputId = "sd_C", 
                         label = "", 
                         value = 200, min = 0, max = 600, step = 5, width = '100%', ticks = FALSE)
      ),
    fluidRow(
             h4("Correlation Between Load and Capacity"),
             sliderInput(inputId = "corr_CL", 
                         label = "", 
                         value = 0, min = -1, max = 1, step = 0.05, width = '100%', ticks = FALSE)
      )
  ),
  
  mainPanel(
    
    h3("Intermediate Calculation"),
    fluidRow(
      column(5, align = "center",
             h4("Mean Margin of Safety")
      ),
      column(5, align = "center",
             h4("Standard Deviation of Margin of Safety")
      ),
      column(2, align = "center",
             h4("Reliability Assessment", align = 'center')
      )
    ),
    fluidRow(
      column(5, align = "center",
             verbatimTextOutput("mean_M")
      ),
      column(5, align = "center",
             verbatimTextOutput("sd_M")
      ),
      column(2, align = "center",
             actionButton(inputId = "update", label = "Calculate")
      )
    ),
    h3("Assessment Result"),
    fluidRow(
      column(4, align = "center",
             h4("P(Margin of Safety < 0)"),
             h6("(Probability of Column Failure)")
      ),
      column(4, align = "center",
             h4("Mean Factor of Safety")
      ),
      column(4, align = "center",
             h4("Construction Cost ($)")
      )
    ),
    fluidRow(
      column(4, align = "center",
             verbatimTextOutput("prob_M")
      ),
      column(4, align = "center",
             verbatimTextOutput("mean_FS")
      ),
      column(4, align = "center",
             verbatimTextOutput("cost")
      )
    ),
    br(),
    fluidRow(
      column(12, align = "center",
             plotOutput("plot_pdf")
      )
    )
  ), position = c("right")
 )
)

server <- function(input, output, session) {
  mean_L = 2500
  sd_L = 700
  
  v <- reactiveValues(
    mean_C = 5300,
    sd_C = 200,
    corr_CL = 0
  )
  
  observeEvent(input$update, {
    v$mean_C <- input$mean_C
    v$sd_C <- input$sd_C
    v$corr_CL <- input$corr_CL
  })
  
  output$mean_M <- renderText({
    input$mean_C - mean_L
  })
  
  output$sd_M <- renderText({
    round(sqrt((input$sd_C)^2 + (sd_L)^2 -2*input$corr_CL*input$sd_C*sd_L), 0)
  })
  
  output$prob_M <- renderText({
    if (pnorm(0,v$mean_C - mean_L,sqrt((v$sd_C)^2 + (sd_L)^2 -2*v$corr_CL*v$sd_C*sd_L)) < 0.0001){ 0.0001 }
    else {round(pnorm(0,v$mean_C - mean_L,sqrt((v$sd_C)^2 + (sd_L)^2 -2*v$corr_CL*v$sd_C*sd_L)), 4)}
  })
  
  output$mean_FS <- renderText({
    v$mean_C / mean_L
  })
  
  output$plot_pdf <- renderPlot({
    mean_C <- v$mean_C
    sd_C <- v$sd_C
    mean_L = 2500
    sd_L = 700
    mean_M <- mean_C - mean_L
    sd_M <- sqrt((sd_C)^2 + (sd_L)^2 -2*v$corr_CL*sd_C*sd_L)
    
    C <- seq(qnorm(0.0001,mean_C,sd_C), qnorm(0.9999,mean_C,sd_C), length.out = 100)
    hC <- dnorm(C,mean_C,sd_C)
    L <- seq(qnorm(0.0001,mean_L,sd_L), qnorm(0.9999,mean_L,sd_L), length.out = 100)
    hL <- dnorm(L,mean_L,sd_L)
    M <- seq(qnorm(0.0001,mean_M,sd_M), qnorm(0.9999,mean_M,sd_M), length.out = 100)
    hM <- dnorm(M,mean_M,sd_M)
    
    dat <- data.frame(Capacity = C, C_pdf = hC, Load = L, L_pdf = hL, Margin = M, M_pdf = hM)
    ggplot(dat, aes(x=Capacity, y=C_pdf)) + 
      geom_line(aes(x=Capacity, y=C_pdf, color='Capacity')) +
      geom_line(aes(x=Load, y=L_pdf, color='Load')) +
      geom_line(aes(x=Margin, y=M_pdf, color='Margin')) +
      geom_vline(aes(xintercept=mean_C, color='Capacity'),linetype="longdash",size=1) +
      geom_vline(aes(xintercept=mean_L, color='Load'), linetype="longdash", size=1) +
      geom_vline(aes(xintercept=mean_M, color='Margin'), linetype="longdash", size=1) +
      geom_vline(aes(xintercept=0, color='Margin'), linetype="longdash", size=1) +
      theme(text = element_text(size=14),legend.title=element_text(size=14)) +
      labs(title ='Probability Density Functions', x='Newtons [N]', y='PDF [units: 1/N]') +
      scale_color_manual(name = NULL, values = c('blue','red','black'))
  })
  
  output$cost <- renderText({
    round(2.2*v$mean_C + 1/(1-exp(-v$sd_C/40000)) + 0.5, -1)
  })
}

shinyApp(ui = ui, server = server)
