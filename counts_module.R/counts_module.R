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
               fileInput(ns("counts_file"), "Upload a normalized counts file (CSV)"),
               sliderInput(ns("variance_slider"), "Variance % Threshold:", min = 0, max = 100, value = 50),
               sliderInput(ns("nonzero_slider"), "Minimum number of non-zero samples", min = 0, max = 100, value = 5),
               actionButton(ns("submit_counts"), "Submit")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel('Summary', tableOutput(ns("summary_table"))),
                 tabPanel('Scatter Plots', 
                          plotOutput(ns("scatter_plot1")),
                          plotOutput(ns("scatter_plot2"))
                 ),
                 tabPanel("Heatmap", plotOutput(ns("clustered_heatmap"))
                 ),
                 tabPanel("PCA", plotOutput(ns("pca_plot"))
                 )
               )
             )
           )
  )
}

# Counts Module Server
counts_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns #namespace for the module
    
    #reactive expression for the processing the uploaded file
    counts_data <- reactive({
      req(input$counts_file) #ensure that the file is uploaded
      read.csv(input$counts_file$datapath, row.names = 1) #THIS MAY NEED CHANGING, HAVE TO SEE
    })
    
    filtered_data <- reactive({
      req(counts_data)
      data <- counts_data
      
      #filter based on the variance threshold
      variance_threshold <- quantile(apply(data, 1, var), probs = input$variance_slider / 100)
      variance_filtered <- data[apply(data, 1, var) >= variance_threshold, ]
      
      #filter based on non-zero samples
      nonzero_threshold <- input$nonzero_slider
      filtered <- filtered_by_variance[rowSums(filtered_by_variance > 0) >= nonzero_threshold, ]
      
      return(filtered)
    })
    
    #summary output table
    output$summary_table <- renderTable({
      req(filtered_data())
      data <- counts_data()
      filtered <- filtered_data()
      
      #calculate counts and percentages
      total_genes <- nrow(data)
      passing_genes <- nrow(filtered)
      not_passing_genes <- total_genes - passing_genes
      passing_percentage <- round((passing_genes / total_genes) * 100, 2)
      not_passing_percentage <- round((not_passing_genes / total_genes) * 100, 2)
      
      #generate summary metrics
      summary <- data.frame(
        Metric = c("Total Samples", "Total Genes", "Genes Passing Filter (%)", "Genes Not Passing Filter (%)"),
        
        Count_and_Percentage = c(
          paste0(ncol(data)),  # Total Samples doesn't need percentages
          paste0(total_genes),  # Total Genes doesn't need percentages
          paste0(passing_genes, " (", passing_percentage, "%)"),
          paste0(not_passing_genes, " (", not_passing_percentage, "%)")
        )
      )
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
