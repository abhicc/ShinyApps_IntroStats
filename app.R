################################################################################
# This app demonstrates the concept of skewness (symmetricity) in terms of 
# histograms and boxplots. It also demonstrates how the mean and standard deviation
# affects these plots, and how numerical summaries are affected by extreme outliers.
################################################################################
# Required libraries
library(sn)
library(tidyverse)
library(mosaic)
library(fGarch)
library(shiny)
################################################################################

library(shiny)
library(ggplot2)
library(readr)

# User interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Summarizing Numerical Data: Visualizing Mean and Median"),
  
  # Tabset
  tabsetPanel(
    tabPanel("Fixed Mean and SD",
             fluidRow(
               column(3,
                      sliderInput(inputId = "mean_value",
                                  label = "Input a mean value",
                                  min = -0,
                                  max = 0,
                                  value = 0,
                                  step = 0.1)),
               column(3,
                      sliderInput(inputId = "sd_value",
                                  label = "Input a standard deviation",
                                  min = 10,
                                  max = 10,
                                  value = 10, 
                                  step = 1)),
               column(3,
                      selectInput(inputId = "shape",
                                  label = "Select a shape",
                                  choices = c("Symmetric", "Positively Skewed", "Negatively Skewed"),
                                  selected = "Symmetric"))
             ),
             fluidRow(
               column(5, 
                      plotOutput("hist")),
               column(5,
                      plotOutput("boxplot")),
               column(2,
                      tableOutput("table"))
             )
    ),
    
    tabPanel("Dynamic Mean and SD",
             fluidRow(
               column(3,
                      sliderInput(inputId = "mean_value_dynamic",
                                  label = "Input a mean value",
                                  min = -100,
                                  max = 100,
                                  value = 0,
                                  step = 0.1)),
               column(3,
                      sliderInput(inputId = "sd_value_dynamic",
                                  label = "Input a standard deviation", 
                                  min = 1,
                                  max = 15,
                                  value = 10, 
                                  step = 1)),
               column(3,
                      selectInput(inputId = "shape_dynamic",
                                  label = "Select a shape",
                                  choices = c("Symmetric", "Positively Skewed", "Negatively Skewed"),
                                  selected = "Symmetric"))
             ),
             fluidRow(
               column(6, 
                      plotOutput("hist_dynamic")),
               column(6,
                      plotOutput("boxplot_dynamic")),
               
             )
    ),
    
    
    tabPanel("Upload File",
             fluidRow(
               fileInput("file", "Upload a file"),
               column(3, selectInput("var", "Select a variable", "")),
               column(3, plotOutput("uploaded_hist")),
               column(3, plotOutput("uploaded_boxplot")),
               column(3, tableOutput("uploaded_table"))
             )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE)
    # Filter quantitative variables
    quantitative_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "var", choices = quantitative_vars)
    return(df)
  })
  
  
  output$uploaded_hist <- renderPlot({
    req(uploaded_data(), input$var)
    ggplot(data = uploaded_data(), aes_string(x = input$var)) +
      geom_histogram(mapping = aes(x = .data[[input$var]], fill = "Histogram"), color = "black", bins = 30) +
      labs(x = "Value", y = "Frequency", title = "Uploaded Histogram") +
      geom_vline(mapping = aes(xintercept = mean(uploaded_data()[[input$var]]), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(uploaded_data()[[input$var]]), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      theme(legend.position = "right")
  })
  
  output$uploaded_boxplot <- renderPlot({
    req(uploaded_data(), input$var)
    ggplot(data = uploaded_data(), aes_string(x = input$var)) +
      geom_boxplot(color = "black", fill = "lightgreen", bins = 30) +
      geom_vline(mapping = aes(xintercept = mean(uploaded_data()[[input$var]]), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(uploaded_data()[[input$var]]), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      labs(x = "Value", y = "input$var", title = "Uploaded Boxplot") +
      theme(legend.position = "right")
  })
  output$uploaded_table <- renderTable({
    req(uploaded_data(), input$var)
    
    # Assuming df() is your uploaded data
    data_summary <- summary(uploaded_data()[[input$var]], na.rm = TRUE)
    
    # Calculate IQR and range
    data_iqr <- IQR(uploaded_data()[[input$var]], na.rm = TRUE)
    data_range <- diff(range(uploaded_data()[[input$var]], na.rm = TRUE))
    
    # Create a data frame with summary statistics
    atable <- data.frame(Summary = c("min", "Q1", "median", "Q3", "max", "IQR", "range"),
                         Values = c(data_summary["Min."], data_summary["1st Qu."], data_summary["Median"],
                                    data_summary["3rd Qu."], data_summary["Max."],
                                    data_iqr,
                                    data_range))
    
    atable
  })
  
  df <- reactive({
    if(input$shape == "Symmetric") {
      val <- rnorm(1000, mean = input$mean_value, sd = input$sd_value)
      df <- data.frame(value = val)
    } else if (input$shape == "Positively Skewed") {
      val_right <- c(rnorm(900, mean = input$mean_value, sd = input$sd_value),
                     runif(100, min = input$mean_value, max = input$mean_value + 100))
      df <- data.frame(value = val_right)
    } else {
      val_left <- c(rnorm(900, mean = input$mean_value, sd = input$sd_value),
                    runif(100, min = input$mean_value - 100, max = input$mean_value))
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  df_dynamic <- reactive({
    if(input$shape_dynamic == "Symmetric") {
      val <- rnorm(1000, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic)
      df <- data.frame(value = val)
    } else if (input$shape_dynamic == "Positively Skewed") {
      val_right <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                     runif(100, min = input$mean_value_dynamic, max = input$mean_value_dynamic + 100))
      df <- data.frame(value = val_right)
    } else {
      val_left <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                    runif(100, min = input$mean_value_dynamic - 100, max = input$mean_value_dynamic))
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  output$hist <- renderPlot({
    ggplot(data = df()) +
      geom_histogram(mapping = aes(x = value, fill = "Histogram"), color = "black", bins = 100) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  output$hist_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_histogram(mapping = aes(x = value, fill = "Histogram"), color = "black", bins = 100) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  output$boxplot <- renderPlot({
    ggplot(data = df()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  output$boxplot_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean")) + # Calculate mean dynamically
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "red", Median = "blue")) +
      scale_fill_manual("", values = c("lightgreen"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  output$table <- renderTable({
    
    res <- favstats(~ value, data=df())
    ftable <- data.frame(Summary = c("min", "Q1", "median", "Q3", "max", "IQR", "range"),
                         Values = c(round(res$min,3), round(res$Q1,3), round(res$median,3), 
                                    round(res$Q3,3), round(res$max,3), 
                                    round(res$Q3,3) - round(res$Q1,3),
                                    round(res$max,3) - round(res$min,3)))
    
    ftable
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
