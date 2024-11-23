#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nick Petrunich BF591 Final Project"),

    # Tab structure
    tabsetPanel(
      
      tabPanel("Samples",
               sidebarLayout(
                 sidebarPanel(
                   fileInput("sample_file", "Upload a sample file (.csv or .tsv)"), #sample_file is the name of the input
                   actionButton("submit_samples", "submit") # button says "submit" and "submit_samples" is the ID of the action button
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary", "Summary content placeholder"),
                     tabPanel("Table", "Table content placeholder"),
                     tabPanel("Plots", "Plot content placeholder")
                   )
                 )
               ))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
