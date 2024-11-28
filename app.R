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
                 fileInput("sample_file", "Upload a sample file (.csv)", accept = ".csv"),  # sample_file is the name of the input
                 actionButton("submit_samples", "Submit")  # Button says "Submit" and "submit_samples" is the ID of the action button
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary", 
                            tableOutput("summary_table")),
                   tabPanel("Table", "Table content placeholder"),
                   tabPanel("Plots", "Plot content placeholder")
                 )
               )
             )
    ),
    
    tabPanel("Counts",
             sidebarLayout(
               sidebarPanel(
                 fileInput("counts_file", "Upload a normalized counts file"),
                 actionButton("submit_counts", "Submit")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          sliderInput("slider_n_variance", "Slider 1:", min = 0, max = 100, value = 50),
                          sliderInput("slider_n_non_zero", "Slider 2:", min = 0, max = 100, value = 50)
                   ),
                   column(6,
                          plotOutput("counts_plot")  # Placeholder for the plot
                   )
                 )
               )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  #for samples tab
  sample_data <- reactive({
    req(input$sample_file) #make sure file is uploaded
    read.csv(input$sample_file$datapath, stringsAsFactors = FALSE)
  })
  
  #generate summary
  output$summary_table <- renderTable({
    req(sample_data)
    
    #get dataframe
    data <- sample_data()
    
    #get summary
    summary <- data.frame(
      `Column Name` = colnames(data),
      `Type` = sapply(data, class),
      `Mean (sd) or Distinct Values` = sapply(data, function(col) {
        if (is.numeric(col)) {
          paste0(round(mean(col, na.rm = TRUE), 2), " (± ", round(sd(col, na.rm = TRUE), 2), ")")
        } else {
          paste(unique(col), collapse = ", ")
        }
      }),
      check.names = FALSE
    )
    
    summary <- rbind(
      c("Number of Rows", "", nrow(data)),
      c("Number of Columns", "", ncol(data)),
      summary
    )
    summary
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
