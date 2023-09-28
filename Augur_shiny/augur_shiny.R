library(shiny)

ui <- tagList(
  fluidPage(
    titlePanel("Augur"),
    sidebarLayout(
      sidebarPanel(
        # uiOutput 做上传文件的 ui, 对应后面的 output$file1
        uiOutput('file1'),
        
        actionButton('reset', 'RESET'),
        hr(),
        downloadButton("downloadData", "Download"),
        hr(),
        h5('Developer:'),
        h6('Small runze (shiny app)'),
        br(),
        h5('Github: '),
        h6('https://github.com/hzaurzli (Small runze)'),
        br(),
        h5('Cition:'),
        h6('https://github.com/neurorestore/Augur')
      ),
      mainPanel(
        h4("Augur table (It will take about 10 mins)"),
        br(),
        br(),
        shinycssloaders::withSpinner(
          dataTableOutput("table")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=1024*1024*1024^2)
  
  values <- reactiveValues(
    file = NULL
  )
  
  
  expression <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file1)) eval(parse(text = load(input$file1$datapath, sessionEnvir)))
  })
  
  
  # observeEvent(input$reset), 代表点击 RESET 时触发的动作,此时重新渲染 fileInput 的 ui
  observeEvent(input$reset, {
    values$file <- NULL
    output$file1 <- renderUI({
      fileInput("file1", "Step 1: Choose scRNA RData",
                accept=c('.RData, .Rds')
      )
    })
  }, ignoreNULL = F)
  
  
  output$table <- renderDataTable({
    library(Augur)
    
    expression <- expression()
    
    if(is.null(expression)){
      warning("Please upload files!")
    } 
    else{
      augur = calculate_auc(expression,n_threads = 16)
      dat <<- data.frame(augur$AUC)
    }
  }, options = list(pageLength = 10))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat,file,row.names = T,quote = F)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

