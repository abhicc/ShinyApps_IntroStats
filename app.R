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
 
# user interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Summarizing Numerical Data: Visualizing Mean and Median"),
  
  # Tabset
  tabsetPanel(
    tabPanel("Fixed Mean and SD",
             fluidRow(
               column(3,
                      selectInput(inputId = "shape",
                                  label = "Select a shape",
                                  choices = c("Symmetric", "Positively Skewed", "Negatively Skewed"),
                                  selected = "Symmetric"))
             ),
             fluidRow(
               column(6, 
                      plotOutput("hist")),
               column(6,
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
                      plotOutput("boxplot_dynamic"))
             )
    ),
    
    
    tabPanel("Upload File",
             fluidRow(
               column(6, align = "left", h4("Upload a CSV file")),
               column(6, align = "left", h4("Choose a Variable")),
               fluidRow(
                 column(6, fileInput("file", "", accept = ".csv")),
                 column(6, selectInput("var", "", ""))
               ),
               fluidRow(
                 column(3, plotOutput("uploaded_hist")),
                 column(3, plotOutput("uploaded_boxplot")),
                 column(3, tableOutput("uploaded_table"))
             )
    )
  )
))


# server function
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
    data <- uploaded_data()
    
    # Filter out missing and non-finite values
    data <- data[complete.cases(data[[input$var]]) & is.finite(data[[input$var]]), ]
    
    ggplot(data = data, aes_string(x = input$var)) +
      geom_histogram(mapping = aes(fill = "Histogram"), color = "black", bins = 30) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      geom_vline(mapping = aes(xintercept = mean(data[[input$var]]), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(data[[input$var]]), color = "Median")) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      theme(legend.position = "none")
  })
  
  output$uploaded_boxplot <- renderPlot({
    data <- uploaded_data()
    
    # Filter out missing and non-finite values
    data <- data[complete.cases(data[[input$var]]) & is.finite(data[[input$var]]), ]
    
    ggplot(data = data, aes_string(x = input$var)) +
      geom_boxplot(color = "black", fill = "#56B4E9") +
      geom_vline(mapping = aes(xintercept = mean(data[[input$var]]), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(data[[input$var]]), color = "Median")) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      labs(x = "Value", y = " ", title = "Boxplot") +
      theme(legend.position = "right")
  })
  
  
 
  output$uploaded_table <- renderTable({
    req(uploaded_data(), input$var)
    
    # Filter out missing values 
    data_summary <- summary(uploaded_data()[[input$var]], na.rm = TRUE)
    data_iqr <- IQR(uploaded_data()[[input$var]], na.rm = TRUE)
    data_range <- diff(range(uploaded_data()[[input$var]], na.rm = TRUE))
    
    atable <- data.frame(Summary = c("min", "Q1", "mean", "median", "Q3", "max", "IQR", "range"),
                         Values = c(data_summary["Min."], data_summary["1st Qu."], data_summary["Mean"], data_summary["Median"],
                                    data_summary["3rd Qu."], data_summary["Max."],
                                    data_iqr,
                                    data_range))
    
    atable
  })
  
  # Create dataframe for fixed tab
  df <- reactive({
    if(input$shape == "Symmetric") {
      # Generate 1000 random #s from a normal dist with fixed mean 0 and sd 10 
      val <- rnorm(1000, mean = 0, sd = 10) 
      df <- data.frame(value = val) # Create df with one col named value, containing the generated random numbers
    } else if (input$shape == "Positively Skewed") {
      # Generate 900 random #s from a normal dist with mean 0 and sd 10 then append 100 uniform random #s (runif) between 0 and 100 to create positive skewness
      val_right <- c(rnorm(900, mean = 0, sd = 10), 
                     runif(100, min = 0, max = 100))
      df <- data.frame(value = val_right)
    } else {
      # Generate 900 random #s from a normal dist with mean 0 and sd 10 then append 100 uniform random #s (runif) between -100 and 0 to create negative skewness
      val_left <- c(rnorm(900, mean = 0, sd = 10),
                    runif(100, min = -100, max = 0))
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  # Create dataframe for dynamic tab
  df_dynamic <- reactive({
    if(input$shape_dynamic == "Symmetric") {
      # Generate 1000 random #s from a normal dist with a mean and sd selected by the user
      val <- rnorm(1000, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic)
      df <- data.frame(value = val)
      # Generate 900 random #s from a normal dist with mean and sd determined by user then append 100 uniform random #s (runif) to create positive skewness
    } else if (input$shape_dynamic == "Positively Skewed") {
      val_right <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                     runif(100, min = input$mean_value_dynamic, max = input$mean_value_dynamic + 100)) # increasing positive skewness
      df <- data.frame(value = val_right)
      # Generate 900 random #s from a normal dist with mean and sd determined by user then append 100 uniform random #s (runif) to create negative skewness
    } else {
      val_left <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                    runif(100, min = input$mean_value_dynamic - 100, max = input$mean_value_dynamic)) # increasing negative skewness
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
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  output$hist_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_histogram(mapping = aes(x = value, fill = "Histogram"), color = "black", bins = 100) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean")) +
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
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
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  output$boxplot_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean")) + # Calculate mean dynamically
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median")) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#0072B2")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  # Summary Table for Fixed Mean and SD tab
  output$table <- renderTable({
    
    res <- favstats(~ value, data=df())
    ftable <- data.frame(Summary = c("min", "Q1", "mean", "median", "Q3", "max", "IQR", "range"),
                         Values = c(round(res$min,3), round(res$Q1,3), round(res$mean,3), round(res$median,3), 
                                    round(res$Q3,3), round(res$max,3), 
                                    round(res$Q3,3) - round(res$Q1,3),
                                    round(res$max,3) - round(res$min,3)))
    
    ftable
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
########################################################################################################