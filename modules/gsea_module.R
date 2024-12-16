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
  ns <- NS(id)
  tabPanel("GSEA",
           tags$div(
             tags$h3("GSEA Results Exploration"),
             tags$p("This interactive module allows users to analyze Gene Set Enrichment Analysis (GSEA) results with three dynamic views.")
           ),
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("gsea_file"), "Upload GSEA results (csv):", accept = c(".csv")),
               actionButton(ns("load_data"), "Submit")
             ),
             mainPanel(
               tabsetPanel(
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
                 tabPanel("Table",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(ns("pvalue_threshold"), "Adjusted P-value Threshold:",
                                          min = 0, max = 1, value = 0.05, step = 0.01),
                              radioButtons(ns("nes_filter"), "Filter by NES:",
                                           choices = c("All" = "all", "Positive NES" = "positive", "Negative NES" = "negative"),
                                           selected = "all"),
                              downloadButton(ns("download_table"), "Download Filtered Table")
                            ),
                            mainPanel(
                              DT::dataTableOutput(ns("gsea_table"))
                            )
                          )),
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
           ))
}

# GSEA Module Server
gsea_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    gsea_data <- reactiveVal(NULL)
    
    observeEvent(input$load_data, {
      req(input$gsea_file)
      data <- read.csv(input$gsea_file$datapath)
      
      if (!all(c("pathway", "padj", "NES") %in% colnames(data))) {
        showNotification("Error: File must contain 'pathway', 'padj', and 'NES' columns.", type = "error")
        return()
      }
      gsea_data(data)
    })
    
    # Filtered data for barplot
    filtered_data <- reactive({
      req(gsea_data())
      gsea_data() %>%
        arrange(padj) %>%
        head(input$num_top_pathways)
    })
    
    # Barplot
    output$gsea_barplot <- renderPlot({
      req(filtered_data())
      ggplot(filtered_data(), aes(x = reorder(pathway, abs(NES)), y = NES, fill = factor(sign(NES), levels = c(-1, 1)))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("-1" = "red", "1" = "blue"), 
                          labels = c("-1" = "Negative NES", "1" = "Positive NES")) +
        labs(
          title = "Top Pathways Ordered by Absolute NES",
          x = "",
          y = "Normalized Enrichment Score (NES)",
          fill = "NES Direction"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "bottom"
        )
    })
    
    # Filtered table
    filtered_table <- reactive({
      req(gsea_data())
      filtered <- gsea_data() %>% filter(padj <= input$pvalue_threshold)
      if (input$nes_filter == "positive") {
        filtered <- filtered %>% filter(NES > 0)
      } else if (input$nes_filter == "negative") {
        filtered <- filtered %>% filter(NES < 0)
      }
      return(filtered)
    })
    
    output$gsea_table <- DT::renderDataTable({
      req(filtered_table())
      DT::datatable(filtered_table(), options = list(pageLength = 10))
    })
    
    output$download_table <- downloadHandler(
      filename = function() { "filtered_gsea_results.csv" },
      content = function(file) {
        write.csv(filtered_table(), file, row.names = FALSE)
      }
    )
    
    # Scatter Plot
    scatter_data <- reactive({
      req(gsea_data())
      gsea_data() %>%
        mutate(log_padj = -log10(padj),
               color = ifelse(padj <= input$scatter_pvalue_threshold, "Meets Threshold", "Below Threshold"))
    })
    
    output$scatter_plot <- renderPlot({
      req(scatter_data())
      ggplot(scatter_data(), aes(x = NES, y = log_padj, color = color)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_manual(values = c("Meets Threshold" = "red", "Below Threshold" = "grey")) +
        labs(
          title = "Scatter Plot of NES vs -log10 Adjusted P-value",
          x = "Normalized Enrichment Score (NES)",
          y = "-log10 Adjusted P-value",
          color = "Threshold Status"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "bottom"
        )
    })
  })
}

# UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    gsea_module_ui("gsea")
  )
)

# Server
server <- function(input, output, session) {
  gsea_module_server("gsea")
}

# Run the App
shinyApp(ui = ui, server = server)
