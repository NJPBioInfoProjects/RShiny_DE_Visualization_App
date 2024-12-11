#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Samples Module UI
library(shiny)
library(DT)  # For interactive data tables

# Samples Module UI
samples_module_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  tabPanel("Samples",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("sample_file"), "Upload a sample file (.csv)", accept = ".csv"),
               actionButton(ns("submit_samples"), "Submit")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Summary", tableOutput(ns("summary_table"))),  # Summary table
                 tabPanel("Table", DT::dataTableOutput(ns("samples_table"))),  # Sortable data table
                 tabPanel("Plots", "Plot content placeholder")  # Placeholder for future plots
               )
             )
           )
  )
}

# Samples Module Server
samples_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to load and process the uploaded sample file
    sample_data <- reactive({
      req(input$sample_file)  # Ensure file is uploaded
      data <- read.csv(input$sample_file$datapath, stringsAsFactors = TRUE)
      return(data)
    })
    
    # Render the Summary Table
    output$summary_table <- renderTable({
      req(sample_data())
      data <- sample_data()
      
      # Create summary statistics for each column
      summary <- data.frame(
        Column_Name = colnames(data),
        Type = sapply(data, function(col) {
          if (is.numeric(col)) {
            "double"  # Change numeric type to "double"
          } else {
            class(col)  # Keep other types as they are
          }
        }),
        `Mean (sd) or Distinct Values` = sapply(data, function(col) {
          if (is.numeric(col)) {
            paste0(round(mean(col, na.rm = TRUE), 2), " (± ", round(sd(col, na.rm = TRUE), 2), ")")
          } else {
            paste(unique(col), collapse = ", ")  # Distinct values for categorical columns
          }
        }),
        check.names = FALSE # Prevents conversion of column names to valid syntax
      )
      return(summary)
    })
    
    # Render the Full Dataset as a Sortable Table
    output$samples_table <- DT::renderDataTable({
      req(sample_data())
      DT::datatable(sample_data(), options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

# Top-level UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    samples_module_ui("samples")  # Use the Samples module
  )
)

# Top-level Server
server <- function(input, output, session) {
  samples_module_server("samples")  # Call the Samples module server
}

# Run the application
shinyApp(ui = ui, server = server)
