
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("BridgeGradeGap"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Input grading data as a csv file.
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
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Input: Ready to Run BAM Model ----
      radioButtons("run_bam", "Run Model",
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