#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Source module files
source("app_samples/samples_module.R")
source("counts_module.R/counts_module.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    samples_module_ui("samples"),  # Load Samples Module
    counts_module_ui("counts")    # Load Counts Module
  )
)

# Define the server
server <- function(input, output, session) {
  samples_module_server("samples")  # Call Samples Server
  counts_module_server("counts")    # Call Counts Server
}

# Run the application 
shinyApp(ui = ui, server = server)
