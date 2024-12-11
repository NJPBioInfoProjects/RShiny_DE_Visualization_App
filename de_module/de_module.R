#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(rlang)
library(readr)
library(shinythemes)

# Define UI for application that draws a histogram
de_module_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "DE",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Load differential expression results for visual analysis",
                  buttonLabel = "Browse", placeholder = "deseq csv"),
        p("A volcano plot can be generated with log2 fold-change on the x-axis and p-adjusted on the y-axis"),
        
        # Dropdown for X-axis and Y-axis selection
        radioButtons(ns("xchoice"), "Choose the column for the x-axis",
                     choices = list("baseMean" = "baseMean",
                                    "log2FoldChange" = "log2FoldChange",
                                    "lfcSE" = "lfcSE",
                                    "stat" = "stat",
                                    "pvalue" = "pvalue",
                                    "padj" = "padj"),
                     selected = "log2FoldChange"),
        radioButtons(ns("ychoice"), "Choose the column for the y-axis",
                     choices = list("baseMean" = "baseMean",
                                    "log2FoldChange" = "log2FoldChange",
                                    "lfcSE" = "lfcSE",
                                    "stat" = "stat",
                                    "pvalue" = "pvalue",
                                    "padj" = "padj"),
                     selected = "padj"),
        
        # Color input for base and highlight colors
        colourInput(ns("base_color"), "Base point color", value = "#22577A"),
        colourInput(ns("highlight_color"), "Highlight point color", value = "#FFCF56"),
        
        # Slider for p-adjust threshold
        sliderInput(ns("slider"), "Select the magnitude of the p-adjusted coloring:",
                    min = -300, max = 0, value = -150),
        
        # Plot button
        actionButton(ns("plot_button"), "Plot")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput(ns("volcano"))),
          tabPanel("Table", tableOutput(ns("table")))
        )
      )
    )
  )
}

# Define server logic required to draw a histogram
de_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to load and process the uploaded data
    load_data <- reactive({
      req(input$file)
      data <- read.csv(input$file$datapath, row.names = 1)
      names(data)[1] <- "gene"  # Rename the first column to "gene"
      return(data)
    })
    
    # Store plot parameters in a reactiveValues object
    plot_params <- reactiveValues(
      slider = NULL,
      x_name = NULL,
      y_name = NULL,
      base_color = NULL,
      highlight_color = NULL,
      initialized = FALSE  # Flag to indicate whether the plot is initialized
    )
    
    # Automatically set initial plot parameters when data is loaded
    observeEvent(load_data(), {
      # Initialize parameters based on current inputs
      plot_params$slider <- input$slider
      plot_params$x_name <- input$xchoice
      plot_params$y_name <- input$ychoice
      plot_params$base_color <- input$base_color
      plot_params$highlight_color <- input$highlight_color
      plot_params$initialized <- TRUE  # Mark as initialized
    })
    
    # Reactive function to generate the volcano plot
    volcano_plot <- function(dataf, x_name, y_name, slider, color1, color2) {
      x_var <- sym(x_name)
      y_var <- sym(y_name)
      threshold <- 10^slider
      dataf$highlight <- dataf[[y_name]] <= threshold
      
      ggplot(dataf, aes(x = !!x_var, y = -log10(!!y_var))) +
        geom_point(aes(color = highlight), size = 1) +
        scale_color_manual(values = c(`FALSE` = color1, `TRUE` = color2, `NA` = 'grey')) +
        labs(
          x = x_name,
          y = paste0("-log10(", y_name, ")"),
          color = paste("padj <", format(threshold, scientific = TRUE))
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
    
    # Reactive function to draw the table
    draw_table <- function(dataf, slider) {
      threshold <- 10^slider
      filtered_data <- dataf[!is.na(dataf$padj) & dataf$padj <= threshold, ]
      filtered_data$pvalue <- formatC(filtered_data$pvalue, format = "e", digits = 5)
      filtered_data$padj <- formatC(filtered_data$padj, format = "e", digits = 5)
      return(filtered_data)
    }
    
    # Reactive expression to generate the plot based on stored plot parameters
    plot_reactive <- reactive({
      req(load_data(), plot_params$initialized)  # Ensure data and initialization
      volcano_plot(
        dataf = load_data(),
        x_name = plot_params$x_name,
        y_name = plot_params$y_name,
        slider = plot_params$slider,
        color1 = plot_params$base_color,
        color2 = plot_params$highlight_color
      )
    })
    
    # Reactive expression to generate the table based on stored plot parameters
    table_reactive <- reactive({
      req(load_data(), plot_params$slider)  # Ensure data and slider value
      draw_table(load_data(), plot_params$slider)
    })
    
    # Update plot parameters only when the "Plot" button is clicked
    observeEvent(input$plot_button, {
      plot_params$slider <- input$slider
      plot_params$x_name <- input$xchoice
      plot_params$y_name <- input$ychoice
      plot_params$base_color <- input$base_color
      plot_params$highlight_color <- input$highlight_color
    })
    
    # Render the volcano plot using the reactive expression
    output$volcano <- renderPlot({
      plot_reactive()
    })
    
    # Render the filtered table using the reactive expression
    output$table <- renderTable({
      table_reactive()
    })
  })
}


# Top-level UI
ui <- fluidPage(
  titlePanel("Nick Petrunich BF591 Final Project"),
  tabsetPanel(
    de_module_ui("DE")  # Use the Counts module
  )
)

# Top-level Server
server <- function(input, output, session) {
  de_module_server("DE")  # Call the Counts module server
}

# Run the application
shinyApp(ui = ui, server = server)

