library(shiny)
#Road Design
eqload = 3

ui <- fluidPage(
  
  h1('Road Design'),
  br(),
    mainPanel(
      fluidRow(
        column(4, align = "center",
               h3("Cost of Road A ($millions)"),
               sliderInput(inputId = "costA", 
                           label = "", 
                           value = 0, min = 0, max = 150,
                           width = '100%'),
               br(),
               h3("Number of Times Road A Fails"),
               verbatimTextOutput("resultA")
        ),
        column(4, align = "center",
               h3("Cost of Road B ($millions)"),
               sliderInput(inputId = "costB", 
                           label = "", 
                           value = 0, min = 0, max = 150,
                           width = '100%'),
               br(),
               h3("Number of Times Road B Fails"),
               verbatimTextOutput("resultB")
        ),
        column(4, align = "center",
               h3("Cost of Road C ($millions)"),
               sliderInput(inputId = "costC", 
                           label = "", 
                           value = 0, min = 0, max = 150,
                           width = '100%'),
               br(),
               h3("Number of Times Road C Fails"),
               verbatimTextOutput("resultC")
        )
      ),
      
      fluidRow(
        column(6, align = "center",
               h3("Number of Times Ambulance"),
               h3("Does Not Get to Hospital"),
               verbatimTextOutput("systemfail")
        ),
        
        column(6, align = "center",
               h3("Total Construction Cost for"),
               h3("Proposed Design ($millions)"),
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
               h3(""),
               h3("Total Number of Earthquake Simulations", align = 'center'),
               numericInput(inputId = "simulnum",
                            label = NULL,
                            width = '100%',
                            value = 0, min = 0, max = 1000),
               h3("Next Trial", align = 'center'),
               actionButton(inputId = "update", label = "Click", width = '100%')
        )
      )
    )
  )

server <- function(input, output, session) {
  v <- reactiveValues(
    realcapA = NULL, 
    realcapB = NULL, 
    realcapC = NULL,
    statusA = NULL,
    statusB = NULL,
    statusC = NULL,
    Afailnum = 0,
    Bfailnum = 0,
    Cfailnum = 0,
    sysfailnum = 0
  )
  
  observeEvent(input$update, {
    v$Afailnum = 0
    v$Bfailnum = 0
    v$Cfailnum = 0
    v$sysfailnum = 0
    
    for (i in 1:input$simulnum) {
      v$realcapA <- rnorm(1,mean = log(input$costA, exp(1)), sd = 1.5)
      v$realcapB <- rnorm(1,mean = log(input$costB, exp(1)), sd = 1.5)
      v$realcapC <- rnorm(1,mean = log(input$costC, exp(1)), sd = 1.5)
      
      v$statusA <- if(v$realcapA < eqload) {
        v$Afailnum <- v$Afailnum + 1
      }
      
      v$statusB <- if(v$realcapB < eqload) {
        v$Bfailnum <- v$Bfailnum + 1
      }
      
      v$statusC <- if(v$realcapC < eqload) {
        v$Cfailnum <- v$Cfailnum + 1
      }
      
      if((v$realcapA < eqload) & (v$realcapB < eqload) & (v$realcapC < eqload)){
        v$sysfailnum <- v$sysfailnum + 1
        } else if ((v$realcapA > eqload) & (v$realcapB < eqload) & (v$realcapC < eqload)){
          v$sysfailnum <- v$sysfailnum + 1
          } else if ((v$realcapA < eqload) & (v$realcapB > eqload) & (v$realcapC < eqload)){
            v$sysfailnum <- v$sysfailnum + 1
            } else if ((v$realcapA < eqload) & (v$realcapB < eqload) & (v$realcapC > eqload)){
              v$sysfailnum <- v$sysfailnum + 1
              } else if ((v$realcapA > eqload) & (v$realcapB > eqload) & (v$realcapC < eqload)){
                
                } else if ((v$realcapA > eqload) & (v$realcapB < eqload) & (v$realcapC > eqload)){
                  
                  } else if ((v$realcapA < eqload) & (v$realcapB > eqload) & (v$realcapC > eqload)){
                    v$sysfailnum <- v$sysfailnum + 1
                    } else if ((v$realcapA > eqload) & (v$realcapB > eqload) & (v$realcapC > eqload)){
                      
                    }
    }
  })
  
  observeEvent(input$reset, {
    v$statusA <- NULL
    v$statusB <- NULL
    v$statusC <- NULL
    v$realcapA <- NULL
    v$realcapB <- NULL
    v$realcapC <- NULL
    v$Afailnum <- 0
    v$Bfailnum <- 0
    v$Cfailnum <- 0
    v$sysfailnum <- 0
  })
  
  output$resultA <- renderText({ 
    c(v$Afailnum)
  })
  output$resultB <- renderText({ 
    c(v$Bfailnum)
  })
  output$resultC <- renderText({ 
    c(v$Cfailnum)
  })
  output$systemfail <- renderText({ 
    c(v$sysfailnum)
  })
  output$totalcost <- renderText({ 
    c(input$costA + input$costB + input$costC)
  })
  
  output$image <- renderImage({
    #w  <- session$clientData$output_image_width
    #h <- session$clientData$output_image_height
    w <- 450
    h <- 300
    
      return(list(
        src = 'A1B1C1.png',
        contentType = 'image/png',
        width = w,
        height = h,
        alt = "ABC"))
    
  }, deleteFile = FALSE)

}

shinyApp(ui = ui, server = server)
