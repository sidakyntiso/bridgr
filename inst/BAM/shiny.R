##Compute bridged score for G bridging observations

library(shiny)

# Source helper functions -----
source("Grading Bias/bridger.R")


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("BridgeGradeGap"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Welcome! This app runs a Bayesian Aldrich Mckelvey model on user data.
              Input grading data as a csv file.
               File should contain one column [ta] that lists all graders.
               Remaining columns [grader1,grader2,..] should be labelled by grader name, 
               as corresponding to values in the first [ta] column"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Ready to Run BAM Model ----
      radioButtons("run_bam", "Run BAM Model",
                   choices = c(Yes = "Run Model",
                               No = "Do Not Run Model"),
                   selected = "Do Not Run Model"),
      downloadButton("downloadData", "Download")
      ),
  
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      #tableOutput("contents")
      
      #textOutput("char")
      
      h3(uiOutput("char")),
      tableOutput("contents"),
      tableOutput("results"),
      textOutput("selected_var")
      
    )
    
    )
  )

# Define server logic to read selected file ----
server <- function(input, output) {

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


# Create Shiny app ----
shinyApp(ui, server)
