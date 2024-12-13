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
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(ns("plot_column"), "Select a column to plot:",
                                          choices = NULL),  # Dynamically populated
                              selectInput(ns("group_column"), "Group by (optional):",
                                          choices = NULL, selected = "None")  # Dynamically populated
                            ),
                            mainPanel(
                              plotOutput(ns("histogram_plot"), height = "600px")
                            )
                          )
                 )
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
      
    # Dynamically update column choices for plotting
    observeEvent(sample_data(), {
      data <- sample_data()
      numeric_columns <- names(data)[sapply(data, is.numeric)]
      categorical_columns <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
      
      # Update dropdown options
      updateSelectInput(session, "plot_column", choices = numeric_columns)
      updateSelectInput(session, "group_column", choices = c("None", categorical_columns))
    })
    
    # Render the histogram plot
    output$histogram_plot <- renderPlot({
      req(sample_data(), input$plot_column)  # Ensure data and a column are selected
      data <- sample_data()
      
      # Extract selected column and optional grouping
      plot_column <- input$plot_column
      group_column <- input$group_column
      
      # Base histogram
      p <- ggplot(data, aes_string(x = plot_column)) +
        geom_histogram(aes(y = ..count..), bins = 10, fill = "cyan", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram of", plot_column), x = plot_column, y = "Counts") +
        theme_minimal()
      
      # Add grouping if selected
      if (group_column != "None") {
        p <- ggplot(data, aes_string(x = plot_column, fill = group_column)) +
          geom_histogram(aes(y = ..count..), bins = 10, color = "black", alpha = 0.7, position = "dodge") +
          labs(title = paste("Histogram of", plot_column, "by", group_column),
               x = plot_column, y = "Counts", fill = group_column) +
          theme_minimal()
      }
      
        return(p)
      
      
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
