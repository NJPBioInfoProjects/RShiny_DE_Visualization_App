#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Counts Module UI
counts_module_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  tabPanel("Counts",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("counts_file"), "Upload a normalized counts file"),
               actionButton(ns("submit_counts"), "Submit")
             ),
             mainPanel(
               fluidRow(
                 column(6,
                        sliderInput(ns("slider1"), "Slider 1:", min = 0, max = 100, value = 50),
                        sliderInput(ns("slider2"), "Slider 2:", min = 0, max = 100, value = 50)
                 ),
                 column(6,
                        plotOutput(ns("counts_plot"))  # Placeholder for the plot
                 )
               )
             )
           )
  )
}

# Counts Module Server
counts_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder plot logic
    output$counts_plot <- renderPlot({
      plot(1:10, rnorm(10), main = "Counts Plot", xlab = "X-axis", ylab = "Y-axis", col = "blue", pch = 16)
    })
  })
}

# Top-level UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    counts_module_ui("counts")  # Use the Counts module
  )
)

# Top-level Server
server <- function(input, output, session) {
  counts_module_server("counts")  # Call the Counts module server
}

# Run the application
shinyApp(ui = ui, server = server)
