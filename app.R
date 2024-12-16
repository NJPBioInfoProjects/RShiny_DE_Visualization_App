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
source("de_module/de_module.R")
source("gsea_module.R/gsea_module.R")
# Define the UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tags$h3("This Shiny application can be used to explore different datasets from the Huntington's disease study. Use data from folder to visualize different analyses"),
  tabsetPanel(
    samples_module_ui("samples"),  # Load Samples Module
    counts_module_ui("counts"),    # Load Counts Module
    de_module_ui("DE"),
    gsea_module_ui("GSEA")
  )
)

# Define the server
server <- function(input, output, session) {
  samples_module_server("samples")  # Call Samples Server
  counts_module_server("counts")    # Call Counts Server
  de_module_server("DE")
  gsea_module_server("GSEA")
}

# Run the application 
shinyApp(ui = ui, server = server)
