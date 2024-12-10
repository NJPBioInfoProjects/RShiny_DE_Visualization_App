#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
                 tabPanel("Summary", tableOutput(ns("summary_table"))),
                 tabPanel("Table", "Table content placeholder"),
                 tabPanel("Plots", "Plot content placeholder")
               )
             )
           )
  )
}

# Samples Module Server
samples_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for Samples
    output$summary_table <- renderTable({
      # Placeholder logic
      data.frame(
        `Column Name` = c("Age", "Condition", "Tau"),
        `Type` = c("double", "factor", "double"),
        `Mean (sd) or Distinct Values` = c("61 (± 15)", "AD, Control", "1401 (± 310)")
      )
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