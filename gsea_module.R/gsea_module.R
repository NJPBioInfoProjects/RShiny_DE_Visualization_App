#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(DT)

# GSEA Module UI
gsea_module_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  tabPanel("GSEA",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("gsea_file"), "Upload GSEA results (CSV/TSV):", accept = c(".csv", ".tsv")),
               actionButton(ns("load_data"), "Submit")  # Button to process the uploaded file
             ),
             mainPanel(
               tabsetPanel(
                 # Tab 1: Barplot
                 tabPanel("Barplot",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(ns("num_top_pathways"), "Number of Top Pathways to Show:",
                                          min = 5, max = 50, value = 10)
                            ),
                            mainPanel(
                              plotOutput(ns("gsea_barplot"))
                            )
                          )),
                 
                 # Tab 2: Sortable Table
                 tabPanel("Table",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(ns("pvalue_threshold"), "Adjusted P-value Threshold:",
                                          min = 0, max = 1, value = 0.05, step = 0.01),
                              radioButtons(ns("nes_filter"), "Filter by NES:",
                                           choices = c("All" = "all", 
                                                       "Positive NES" = "positive", 
                                                       "Negative NES" = "negative"),
                                           selected = "all"),
                              downloadButton(ns("download_table"), "Download Filtered Table")
                            ),
                            mainPanel(
                              DT::dataTableOutput(ns("gsea_table"))
                            )
                          )),
                 
                 # Tab 3: Scatter Plot
                 tabPanel("Scatter Plot",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(ns("scatter_pvalue_threshold"), "Adjusted P-value Threshold:",
                                          min = 0, max = 1, value = 0.05, step = 0.01)
                            ),
                            mainPanel(
                              plotOutput(ns("scatter_plot"))
                            )
                          ))
               )
             )
           )
  )
}

# GSEA Module Server
gsea_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    gsea_data <- reactiveVal(NULL)
    
    # Load and validate data
    observeEvent(input$load_data, {
      req(input$gsea_file)
      ext <- tools::file_ext(input$gsea_file$datapath)
      data <- if (ext == "csv") {
        read.csv(input$gsea_file$datapath)
      } else {
        read.delim(input$gsea_file$datapath)
      }
      # Check for required columns
      if (!all(c("pathway", "padj", "NES") %in% colnames(data))) {
        showNotification("Error: File must contain 'pathway', 'padj', and 'NES' columns.", type = "error")
        return()
      }
      gsea_data(data)
    })
    
    # Filtered and ordered data for barplot
    filtered_data <- reactive({
      req(gsea_data())
      gsea_data() %>%
        arrange(padj) %>%        # Order by padj (ascending)
        head(input$num_top_pathways)  # Display top N pathways (slider input)
    })
    
    # Render the bar plot
    output$gsea_barplot <- renderPlot({
      req(filtered_data())
      ggplot(filtered_data(), aes(x = reorder(pathway, abs(NES)), y = NES, fill = NES > 0)) +
        geom_bar(stat = "identity") +
        coord_flip() +  # Horizontal bars
        scale_fill_manual(values = c("red", "blue"), labels = c("Negative NES", "Positive NES")) +
        labs(
          title = "Top Pathways Ordered by Absolute NES",
          x = "",
          y = "Normalized Enrichment Score (NES)",
          fill = "NES Direction"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 8),  # Adjust y-axis label size
          legend.position = "bottom"
        )
    })
    
    # Filtered table based on p-value and NES direction
    filtered_table <- reactive({
      req(gsea_data())
      data <- gsea_data()
      
      # Filter by p-value threshold
      filtered <- data %>% filter(padj <= input$pvalue_threshold)
      
      # Filter by NES direction
      if (input$nes_filter == "positive") {
        filtered <- filtered %>% filter(NES > 0)
      } else if (input$nes_filter == "negative") {
        filtered <- filtered %>% filter(NES < 0)
      }
      return(filtered)
    })
    
    # Render the filtered table
    output$gsea_table <- DT::renderDataTable({
      req(filtered_table())
      DT::datatable(filtered_table(), options = list(pageLength = 10))
    })
    
    # Download filtered table
    output$download_table <- downloadHandler(
      filename = function() { "filtered_gsea_results.csv" },
      content = function(file) {
        write.csv(filtered_table(), file, row.names = FALSE)
      }
    )
  })
}

# Top-level UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    gsea_module_ui("gsea")  # GSEA module
  )
)

# Top-level Server
server <- function(input, output, session) {
  gsea_module_server("gsea")  # GSEA module
}

# Run the Shiny app
shinyApp(ui = ui, server = server)