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

options(shiny.maxRequestSize = 100 * 1024^2)
# Counts Module UI
counts_module_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  tabPanel("Counts",
           tags$div(
             tags$h3("Counts Matrix Exploration"),  # Title
             tags$p("This module allows  allows you to filter and explore gene expression data through multiple tools, including summary statistics, diagnostic scatter plots, clustered heatmaps, and principal component analysis (PCA). Adjust filtering thresholds to observe their effects on the dataset and identify key patterns or trends.")  # Caption
           ),
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("counts_file"), "Upload a normalized counts file (CSV)"),
               sliderInput(ns("variance_slider"), "Variance % Threshold:", min = 0, max = 100, value = 50),
               sliderInput(ns("nonzero_slider"), "Minimum number of non-zero samples", min = 0, max = 8, value = 5)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel('Summary', tableOutput(ns("summary_table"))),
                 tabPanel('Scatter Plots', 
                          plotOutput(ns("scatter_plot1")),
                          plotOutput(ns("scatter_plot2"))
                 ),
                 tabPanel("Heatmap", 
                          p("Please allow 10-20 seconds for heatmap to load"),
                          plotOutput(ns("heatmap_plot"))
                 ),
                 tabPanel("PCA",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(ns("pc_x"), "X-axis Principal Component", 
                                          choices = 1:10, selected = 1),  # Top 10 PCs as options
                              selectInput(ns("pc_y"), "Y-axis Principal Component", 
                                          choices = 1:10, selected = 2)   # Default PC2 for y-axis
                            ),
                            mainPanel(
                              plotOutput(ns("pca_plot"), height = "600px")  # PCA Scatter Plot
                            )
                          )
                 )
               )
             )
           )
  )
}


# Counts Module Server
counts_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module
    
    # Reactive expression for processing the uploaded file
    counts_data <- reactive({
      req(input$counts_file)  # Ensure that the file is uploaded
      data <- read.csv(input$counts_file$datapath, row.names = 1)  # THIS MAY NEED CHANGING, HAVE TO SEE
      return(data)
    })
    
    # Reactive expression for filtered data
    filtered_data <- reactive({
      req(input$counts_file)
      data <- counts_data()
      
      # Filter based on the variance threshold
      variance_threshold <- quantile(apply(data, 1, var), probs = input$variance_slider / 100)
      filtered_by_variance <- data[apply(data, 1, var) >= variance_threshold, ]
      
      # Filter based on non-zero samples
      nonzero_threshold <- input$nonzero_slider
      filtered <- filtered_by_variance[rowSums(filtered_by_variance > 0) >= nonzero_threshold, ]      
      
      return(filtered)
    })
    
    # Summary output table
    output$summary_table <- renderTable({
      req(filtered_data())
      data <- counts_data()
      filtered <- filtered_data()
      
      # Calculate counts and percentages
      total_genes <- nrow(data)
      passing_genes <- nrow(filtered)
      not_passing_genes <- total_genes - passing_genes
      passing_percentage <- round((passing_genes / total_genes) * 100, 2)
      not_passing_percentage <- round((not_passing_genes / total_genes) * 100, 2)
      
      # Generate summary metrics
      summary <- data.frame(
        Metric = c("Total Samples", "Total Genes", "Genes Passing Filter (%)", "Genes Not Passing Filter (%)"),
        
        Count_and_Percentage = c(
          paste0(ncol(data)),  # Total Samples doesn't need percentages
          paste0(total_genes),  # Total Genes doesn't need percentages
          paste0(passing_genes, " (", passing_percentage, "%)"),
          paste0(not_passing_genes, " (", not_passing_percentage, "%)")
        )
      )
      return(summary)
    })
    
    # Scatter plot 1: Median Count vs Variance
    output$scatter_plot1 <- renderPlot({
      req(filtered_data())
      data <- counts_data()  # Full dataset before filtering
      filtered <- filtered_data()  # Filtered dataset
      
      # Calculate median counts and variances
      df <- data.frame(
        Median = apply(data, 1, median, na.rm = TRUE),
        Variance = apply(data, 1, var, na.rm = TRUE),
        PassedFilter = rownames(data) %in% rownames(filtered)  # TRUE if gene passed filters
      )
      
      # Generate the plot with log scale
      ggplot(df, aes(x = Variance, y = Median, color = PassedFilter)) +
        geom_point(size = 2) +
        scale_x_log10() +  # Apply log scale to the x-axis
        scale_y_log10() +  # Apply log scale to the y-axis
        scale_color_manual(values = c("yellow", "darkblue"), labels = c("Filtered Out", "Passed Filter")) +
        labs(
          title = "Median Count vs Variance (Log Scale)",
          x = "Variance (log10)",
          y = "Median Count (log10)",
          color = "Filter Status"
        ) +
        theme_minimal()
    })
    
    # Scatter plot 2: Median Count vs Number of Zeros
    output$scatter_plot2 <- renderPlot({
      req(filtered_data())
      data <- counts_data()  # Full dataset before filtering
      filtered <- filtered_data()  # Filtered dataset
      
      # Calculate median counts and number of zeros
      df <- data.frame(
        Median = as.numeric(apply(data, 1, median, na.rm = TRUE)),  # Ensure it's numeric
        NumZeros = as.numeric(apply(data, 1, function(x) sum(x == 0))),  # Ensure it's numeric
        PassedFilter = as.logical(rownames(data) %in% rownames(filtered))  # Ensure logical type
      )
      
      # Generate the plot
      ggplot(df, aes(x = NumZeros, y = Median, color = PassedFilter)) +
        geom_point(size = 2) +
        scale_y_log10() +  # Apply log scale to the y-axis (median counts)
        scale_color_manual(values = c("lightgray", "darkgreen"), labels = c("Filtered Out", "Passed Filter")) +
        labs(
          title = "Median Count vs Number of Zeros",
          x = "Number of Zero",
          y = "Median Count (log10)",
          color = "Filter Status"
        ) +
        theme_minimal()
    })
    
    # Heatmap plot
    output$heatmap_plot <- renderPlot({
      req(filtered_data())
      data <- filtered_data()  # Use filtered data
      
      # Apply log transformation for better visualization
      log_data <- log1p(data)  # Log-transform counts (log(x + 1))
      
      # Generate the heatmap
      library(gplots)
      heatmap.2(
        x = as.matrix(log_data),  # Convert data to matrix
        dendrogram = "both",  # Cluster both rows and columns
        trace = "none",  # Remove trace lines
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Heatmap colors
        scale = "row",  # Scale by row (standardize genes)
        margins = c(8, 8),  # Margins for row/column labels
        key = TRUE,  # Show color key
        key.title = "Log Count",  # Title for the color key
        key.xlab = "Log-Transformed Value",  # X-axis label for key
        main = "Clustered Heatmap of Filtered Counts",  # Title
        density.info = "none"  # Disable density plot in the color key
      )
    })
    
    #PCA plot
    output$pca_plot <- renderPlot({
      req(filtered_data())  # Ensure filtered data is available
      data <- filtered_data()  # Get the filtered dataset
      
      # Perform PCA
      pca <- prcomp(t(data), scale. = TRUE)  # Transpose to have samples as rows
      
      # Calculate % variance explained for each PC
      var_explained <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 2)
      
      # Extract user-selected principal components
      pc_x <- as.numeric(input$pc_x)  # PC for x-axis
      pc_y <- as.numeric(input$pc_y)  # PC for y-axis
      
      # Check if the selected PCs exist in var_explained
      if (pc_x > length(var_explained) || pc_y > length(var_explained)) {
        return(NULL)  # Return nothing if PCs are out of range
      }
      
      # Create a data frame for plotting
      pca_data <- as.data.frame(pca$x)
      pca_data$Sample <- rownames(pca_data)  # Add sample names for labeling
      
      # Generate the scatter plot
      ggplot(pca_data, aes_string(x = paste0("PC", pc_x), y = paste0("PC", pc_y), label = "Sample")) +
        geom_point(size = 3, color = "darkblue") +
        geom_text(vjust = -1, size = 3) +  # Optional: Add sample labels
        labs(
          title = "PCA Scatter Plot",
          x = paste0("PC", pc_x, " (", var_explained[pc_x], "% Variance Explained)"),
          y = paste0("PC", pc_y, " (", var_explained[pc_y], "% Variance Explained)")
        ) +
        theme_minimal()
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


