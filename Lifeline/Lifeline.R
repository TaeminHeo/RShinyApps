library(shiny)
# Lifeline
eqload = 3

ui <- fluidPage(
  
  h1('Lifeline'),
  br(),
  mainPanel(
  fluidRow(
    column(4, align = "center",
      h3("Cost of Road A ($millions)"),
      sliderInput(inputId = "costA", 
            label = "", 
            value = 0, min = 0, max = 150,
            width = "100%"),
      br(),
      h3("Road A"),
      verbatimTextOutput("resultA")
    ),
    column(4, align = "center",
      h3("Cost of Road B ($millions)"),
      sliderInput(inputId = "costB", 
                  label = "", 
                  value = 0, min = 0, max = 150,
                  width = "100%"),
      br(),
      h3("Road B"),
      verbatimTextOutput("resultB")
    ),
    column(4, align = "center",
      h3("Cost of Road C ($millions)"),
      sliderInput(inputId = "costC", 
            label = "", 
            value = 0, min = 0, max = 150,
            width = "100%"),
      br(),
      h3("Road C"),
      verbatimTextOutput("resultC")
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
  ))
)

server <- function(input, output, session) {
  v <- reactiveValues(
    realcapA = NULL, 
    realcapB = NULL, 
    realcapC = NULL,
    statusA = c("Still intact"),
    statusB = c("Still intact"),
    statusC = c("Still intact"),
    trialnum = 0
    )
  
  observeEvent(input$update, {
    v$realcapA <- rnorm(1,mean = log(input$costA, exp(1)), sd = 1.5)
  })
  observeEvent(input$update, {
    v$realcapB <- rnorm(1,mean = log(input$costB, exp(1)), sd = 1.5)
  }) 
  observeEvent(input$update, {
    v$realcapC <- rnorm(1,mean = log(input$costC, exp(1)), sd = 1.5)
  }) 
  
  observeEvent(input$update, {
    v$statusA <- if(v$realcapA < eqload) {
      c("Destroyed!")
    } else {
      c("Still intact")
    }
  })
  
  observeEvent(input$update, {
    v$statusB <- if(v$realcapB < eqload) {
      c("Destroyed!")
    } else {
      c("Still intact")
    }
  })
  
  observeEvent(input$update, {
    v$statusC <- if(v$realcapC < eqload) {
      c("Destroyed!")
    } else {
      c("Still intact")
    }
  })
  
  observeEvent(input$update, {
    v$trialnum <- v$trialnum + 1
  })
  
  observeEvent(input$reset, {
    v$statusA <- c("Still intact")
    v$statusB <- c("Still intact")
    v$statusC <- c("Still intact")
    v$realcapA <- NULL
    v$realcapB <- NULL
    v$realcapC <- NULL
    v$trialnum <- 0
  })
  
  output$resultA <- renderText({ 
    v$statusA
  })
  output$resultB <- renderText({ 
    v$statusB
  })
  output$resultC <- renderText({ 
    v$statusC
  })
  
  output$value <- renderText({ v$trialnum })
  
  output$image <- renderImage({
    #w  <- session$clientData$output_image_width
    #h <- session$clientData$output_image_height
    w <- 450
    h <- 300
    
    if(v$trialnum == 0){
      return(list(
        src = 'A1B1C1.png',
        contentType = 'image/png',
        width = w,
        height = h,
        alt = "ABC"
      ))} 
    
    if((v$realcapA < eqload) & (v$realcapB < eqload) & (v$realcapC < eqload)){
      return(list(
        src = 'A0B0C0.png',
        contentType = 'image/png',
        width = w,
        height = h,
        alt = "A'B'C'"
      ))} else if ((v$realcapA > eqload) & (v$realcapB < eqload) & (v$realcapC < eqload)){
        return(list(
          src = 'A1B0C0.png',
          contentType = 'image/png',
          width = w,
          height = h,
          alt = "AB'C'"
        ))} else if ((v$realcapA < eqload) & (v$realcapB > eqload) & (v$realcapC < eqload)){
          return(list(
            src = 'A0B1C0.png',
            contentType = 'image/png',
            width = w,
            height = h,
            alt = "A'BC'"
          ))} else if ((v$realcapA < eqload) & (v$realcapB < eqload) & (v$realcapC > eqload)){
            return(list(
              src = 'A0B0C1.png',
              contentType = 'image/png',
              width = w,
              height = h,
              alt = "A'B'C"
            ))} else if ((v$realcapA > eqload) & (v$realcapB > eqload) & (v$realcapC < eqload)){
              return(list(
                src = 'A1B1C0.png',
                contentType = 'image/png',
                width = w,
                height = h,
                alt = "ABC'"
              ))} else if ((v$realcapA > eqload) & (v$realcapB < eqload) & (v$realcapC > eqload)){
                return(list(
                  src = 'A1B0C1.png',
                  contentType = 'image/png',
                  width = w,
                  height = h,
                  alt = "AB'C"
                ))} else if ((v$realcapA < eqload) & (v$realcapB > eqload) & (v$realcapC > eqload)){
                  return(list(
                    src = 'A0B1C1.png',
                    contentType = 'image/png',
                    width = w,
                    height = h,
                    alt = "A'BC"
                  ))} else if ((v$realcapA > eqload) & (v$realcapB > eqload) & (v$realcapC > eqload)){
                    return(list(
                      src = 'A1B1C1.png',
                      contentType = 'image/png',
                      width = w,
                      height = h,
                      alt = "ABC"
                    ))}
                      
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
