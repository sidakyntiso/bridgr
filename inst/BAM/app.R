# Define server logic to read selected file ----
server <- function(input, output) {
  # Source helper functions -----
  source("bridger.R")
  
  input_table <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
  })
  
  output$contents <- renderTable({
    if(input$disp == "head") {
      return(head(input_table()))
    }
    else {
      return(input_table())
    }
  })
  
  # Value for model run ----
  output$selected_var <- renderText({ 
    paste("You have selected", input$run_bam)
  })
  
  # Reactive value for input dataset ----
  output$char <- renderUI({
    df <- input_table()
    HTML(paste(paste0("Number of exams :", nrow(df)), 
               paste0("Number of graders :", ncol(df)-1),
               paste0("Number of bridging exams :", 
                      nrow(subset(df,df[,2]==df[,3]&df[,2]==df[,4]))),sep="<br/>"))
    
    
  })
  
  # Reactive value of output dataset ----
  bam_results <- reactive({
    df <- input_table()
    if(input$run_bam == "Run Model") {
      results <- bam(df=df)
      return(results)
    }
  })
  
  output$results <- renderTable({
    if(input$disp == "head") {
      return(head(bam_results()))
    }
    else {
      return(bam_results())
    }
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("corrected_grades", '.csv', sep='')
    },
    content = function(file) {
      
      write.csv(bam_results(), file)
    }
  )
  
  
  
}