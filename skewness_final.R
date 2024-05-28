################################################################################
# This app demonstrates the concept of skewness (symmetricity) in terms of 
# histograms and boxplots. It also demonstrates how the mean and standard deviation
# affect these plots, and how numerical summaries are affected by extreme outliers.
################################################################################

setwd("/cloud/project/Preuploaded datasets for app.R")

HappyPlanetIndex <- read.csv("HappyPlanetIndex.csv", row.names = 1)
FloridaLakes <- read.csv("FloridaLakes.csv", row.names = 1)
SleepStudy <- read.csv("SleepStudy.csv", row.names = 1)
StudentSurvey <- read.csv("StudentSurvey.csv", row.names = 1)
USStates <- read.csv("USStates1e.csv", row.names = 1)
HomesForSale <- read.csv("HomesForSale.csv", row.names = 1)
HappyPlanetIndex <- HappyPlanetIndex[complete.cases(HappyPlanetIndex), ]
FloridaLakes <- FloridaLakes[complete.cases(FloridaLakes), ]
SleepStudy <- SleepStudy[complete.cases(SleepStudy), ]
StudentSurvey <- StudentSurvey[complete.cases(StudentSurvey), ]
USStates <- USStates[complete.cases(USStates), ]
HomesForSale <- HomesForSale[complete.cases(HomesForSale), ]

# Set CRAN mirror
options(repos = c(CRAN = 'https://cran.rstudio.com/'))

# Load necessary libraries
packages <- c("tidyverse", "shiny", "rsconnect", "MASS", "sn", "patchwork", 
              "mosaic", "fGarch", "gridExtra")

# Check if packages are installed; if not, install them
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# Install and load packages
install_if_missing(packages)

# Example Shiny app
library(shiny)

# User Interface
ui <- fluidPage(
  titlePanel("Summarizing Numerical Data: Visualizing Mean and Median"),
  # Tab for Fixed Mean and SD
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
               column(6, plotOutput("hist")),
               column(6, plotOutput("boxplot")),
               column(2, tableOutput("table"))
             ),
             fluidRow(
               column(
                 width = 12,
                 p("App created by Nina Austria and Andrea Zantek and maintained by Abhishek Chakraborty", align = "center", style = "margin-top: 20px;")
               )
             )
    ),
    # Tab for Dynamic Mean and SD
    tabPanel("Dynamic Mean and SD",
             fluidRow(
               column(3, sliderInput(inputId = "mean_value_dynamic",
                                     label = "Input a mean value",
                                     min = -50,
                                     max = 50,
                                     value = 0,
                                     step = 0.1)),
               column(3, sliderInput(inputId = "sd_value_dynamic",
                                     label = "Input a standard deviation", 
                                     min = 1,
                                     max = 15,
                                     value = 10, 
                                     step = 1)),
               column(3, selectInput(inputId = "shape_dynamic",
                                     label = "Select a shape",
                                     choices = c("Symmetric", "Positively Skewed", "Negatively Skewed"),
                                     selected = "Symmetric"))
             ),
             fluidRow(
               column(6, plotOutput("hist_dynamic")),
               column(6, plotOutput("boxplot_dynamic"))
             ),
             fluidRow(
               column(
                 width = 12,
                 p("App created by Nina Austria and Andrea Zantek and maintained by Abhishek Chakraborty", align = "center", style = "margin-top: 20px;")
               )
             )
    ),
    # Tab for Preloaded Datasets
    tabPanel("Preloaded Dataset",
             fluidRow(
               column(12, HTML("<br>")), # Add a blank row
             ),
             fluidRow(
               column(5,
                      sidebarPanel(
                        style = "width: 300px;", # Adjust the width here
                        uiOutput("dataset_select"),
                        uiOutput("variable_select"),
                        downloadButton("download_data", "Download Plot as JPEG")  # Add download button
                      )),
               column(7,
                      uiOutput("data_plot"),
                      tableOutput("data_table")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 p("App created by Nina Austria and Andrea Zantek and maintained by Abhishek Chakraborty", align = "center", style = "margin-top: 20px;")
               )
             )
    )
  )
)

# Server section
server <- function(input, output, session) {
  # Preloaded dataset
  output$dataset_select <- renderUI({
    return(selectInput("dataset", "Choose a Dataset", choices = c("HappyPlanetIndex", "USStates", "SleepStudy", "StudentSurvey", "HomesForSale", "FloridaLakes" ), selected = "HappyPlanetIndex"))
  })
  
  output$variable_select <- renderUI({
    req(input$dataset)
    dataset <- switch(input$dataset,
                      "HappyPlanetIndex" = HappyPlanetIndex,
                      "USStates" = USStates,
                      "SleepStudy" = SleepStudy,
                      "StudentSurvey" = StudentSurvey,
                      "HomesForSale" = HomesForSale,
                      "FloridaLakes" = FloridaLakes)
    return(selectInput("var", "Choose a Variable", choices = names(dataset)))
  })
  
  output$data_plot <- renderUI({
    plotOutput("dataset_plot", width = "100%", height = "400px")
  })
  
  output$data_table <- renderTable({
    req(input$var, input$dataset)
    dataset <- switch(input$dataset,
                      "HappyPlanetIndex" = HappyPlanetIndex,
                      "USStates" = USStates,
                      "SleepStudy" = SleepStudy,
                      "StudentSurvey" = StudentSurvey,
                      "HomesForSale" = HomesForSale,
                      "FloridaLakes" = FloridaLakes)
    data_column <- dataset[[input$var]]
    
    if (is.numeric(data_column)) {
      if (length(unique(data_column)) <= 10) {
        # Generate frequency table for categorical variable after removing NA values
        data_column <- na.omit(data_column)
        freq_table <- table(data_column)
        freq_df <- as.data.frame(freq_table)
        colnames(freq_df) <- c("Category", "Frequency")  # Correct column names
        return(freq_df)
      } else {
        # Generate summary table for numeric variable
        data_summary <- summary(data_column, na.rm = TRUE)
        data_iqr <- IQR(data_column, na.rm = TRUE)
        data_range <- diff(range(data_column, na.rm = TRUE))
        missing_count <- sum(is.na(data_column))
        atable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range","Number of NA Observations"),
                             Values = c(data_summary["Min."], data_summary["1st Qu."], mean(data_column, na.rm = TRUE), data_summary["Median"],
                                        data_summary["3rd Qu."], data_summary["Max."],
                                        data_iqr,
                                        data_range,
                                        missing_count))
        return(atable)
      }
    } else {
      # Generate frequency table for categorical variable after removing NA values
      data_column <- na.omit(data_column)
      freq_table <- table(data_column)
      freq_df <- as.data.frame(freq_table)
      if (nrow(freq_df) > 0) {
        if (ncol(freq_df) < 2) {
          freq_df$Frequency <- NA  # Add a Frequency column if not present
        }
        colnames(freq_df) <- c("Category", "Frequency")  # Correct column names
      }
      return(freq_df)
    }
  })
  
  # Plot for preloaded dataset tab
  output$dataset_plot <- renderPlot({
    req(input$var)
    dataset <- switch(input$dataset,
                      "HappyPlanetIndex" = HappyPlanetIndex,
                      "USStates" = USStates,
                      "SleepStudy" = SleepStudy,
                      "StudentSurvey" = StudentSurvey,
                      "HomesForSale" = HomesForSale,
                      "FloridaLakes" = FloridaLakes)
    data_column <- dataset[[input$var]]
    if (!is.null(data_column)) { # Check if data_column is not NULL
      data <- dataset[complete.cases(data_column) & !is.na(data_column), ]
      if (!is.factor(data_column) && is.numeric(data_column) && length(unique(data_column)) > 10) {        # Existing histogram and boxplot code...
        p <- ggplot(data = data, aes_string(x = input$var)) +
          geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 30) +
          geom_density(color = "black", alpha = 0.5, size = 1.5) +
          labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
          geom_vline(mapping = aes(xintercept = mean(.data[[input$var]]), color = "Mean"), size = 2) +
          geom_vline(mapping = aes(xintercept = median(.data[[input$var]]), color = "Median"), size = 2) +
          scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
          scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
          theme(legend.position = "none")
        bp <-  ggplot(data = data, aes_string(x = input$var)) +
          geom_boxplot(color = "black", fill = "#56B4E9") +
          geom_vline(mapping = aes(xintercept = mean(.data[[input$var]]), color = "Mean"), size = 2) +
          geom_vline(mapping = aes(xintercept = median(.data[[input$var]]), color = "Median"), size = 2) +
          scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
          scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
          labs(x = "Value", y = " ", title = "Boxplot") +
          theme(legend.position = "right")
        # Set the height of each plot
        p <- p + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
        bp <- bp + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
        # Return the combined plots
        return(grid.arrange(p, bp, nrow = 1))
      } else {
        # New block for categorical variables
        p <- ggplot(data = data, aes_string(x = input$var)) +
          geom_bar(fill = "#56B4E9") +
          labs(x = "Category", y = "Frequency", title = "Barplot for Categorical Variable") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        return(p)
      }
    }
  })
  
  # Download plot
  output$download_data <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".jpeg", sep = "")
    },
    content = function(file) {
      if (!is.null(input$var)) {
        if (!is.null(input$dataset)) {
          dataset <- switch(input$dataset,
                            "HappyPlanetIndex" = HappyPlanetIndex,
                            "USStates" = USStates,
                            "SleepStudy" = SleepStudy,
                            "StudentSurvey" = StudentSurvey,
                            "HomesForSale" = HomesForSale,
                            "FloridaLakes" = FloridaLakes)
          data_column <- dataset[[input$var]]
          if (!is.null(data_column)) { 
            if (!is.factor(data_column) && is.numeric(data_column) && length(unique(data_column)) > 10) {        
              p <- ggplot(data = dataset, aes_string(x = input$var)) +
                geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 30) +
                geom_density(color = "black", alpha = 0.5, size = 1.5) +
                labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
                geom_vline(mapping = aes(xintercept = mean(.data[[input$var]]), color = "Mean"), size = 2) +
                geom_vline(mapping = aes(xintercept = median(.data[[input$var]]), color = "Median"), size = 2) +
                scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
                scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
                theme(legend.position = "none")
              bp <-  ggplot(data = dataset, aes_string(x = input$var)) +
                geom_boxplot(color = "black", fill = "#56B4E9") +
                geom_vline(mapping = aes(xintercept = mean(.data[[input$var]]), color = "Mean"), size = 2) +
                geom_vline(mapping = aes(xintercept = median(.data[[input$var]]), color = "Median"), size = 2) +
                scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
                scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
                labs(x = "Value", y = " ", title = "Boxplot") +
                theme(legend.position = "right")
              p <- p + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
              bp <- bp + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
              jpeg(file = file)
              grid.arrange(p, bp, nrow = 1)
              dev.off()
            } else {
              p <- ggplot(data = dataset, aes_string(x = input$var)) +
                geom_bar(fill = "#56B4E9") +
                labs(x = "Category", y = "Frequency", title = "Barplot for Categorical Variable") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              jpeg(file = file)
              print(p)
              dev.off()
            }
          }
        }
      }
    }
  )
  
  
  
        

  # Fixed mean and SD
  set.seed(422024)
  df <- reactive({
    if(input$shape == "Symmetric") {
      val <- rnorm(1000, mean = 0, sd = 10) 
      df <- data.frame(value = val)
    } else if (input$shape == "Positively Skewed") {
      val_right <- c(rnorm(900, mean = 0, sd = 10), 
                     runif(500, min = 0, max = 25),
                     runif(400, min = 25, max = 50),
                     runif(50, min = 50, max = 75),
                     runif(300, min = 50, max = 100))
      df <- data.frame(value = val_right)
    } else {
      val_left <- c(rnorm(900, mean = 0, sd = 10),
                    runif(500, min = -25, max = 0), 
                    runif(400, min = -50, max = -25), 
                    runif(50, min = -75, max = -50), 
                    runif(300, min = -100, max = -50))
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  # Dynamic mean and SD
  df_dynamic <- reactive({
    if(input$shape_dynamic == "Symmetric") {
      val <- rnorm(1000, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic)
      df <- data.frame(value = val)
    } else if (input$shape_dynamic == "Positively Skewed") {
      val_right <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                     runif(500, min = input$mean_value_dynamic, max = input$mean_value_dynamic+25), 
                     runif(400, min = input$mean_value_dynamic+25, max = input$mean_value_dynamic+50), 
                     runif(50, min = input$mean_value_dynamic+50, max = input$mean_value_dynamic+75), 
                     runif(300, min = input$mean_value_dynamic+50, max = input$mean_value_dynamic+100))
      df <- data.frame(value = val_right)
    } else {
      val_left <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                    runif(500, min = input$mean_value_dynamic-25, max = input$mean_value_dynamic), 
                    runif(400, min = input$mean_value_dynamic-50, max = input$mean_value_dynamic-25), 
                    runif(50, min = input$mean_value_dynamic-75, max = input$mean_value_dynamic-50), 
                    runif(300, min = input$mean_value_dynamic-100, max = input$mean_value_dynamic-50))
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  # Histogram for fixed mean and SD
  output$hist <- renderPlot({
    ggplot(data = df(), aes(x = value)) +
      geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 45) +
      geom_density(color = "black", alpha = 0.5, size = 1.5) +
      labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      theme(legend.position = "none")
  })
  
  # Boxplot for fixed mean and SD
  output$boxplot <- renderPlot({
    ggplot(data = df()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  # Summary table for fixed mean and SD
  output$table <- renderTable({
    data <- summary(df()$value)
    data_iqr <- IQR(df()$value, na.rm = TRUE)  # Handle missing values
    data_range <- diff(range(df()$value, na.rm = TRUE))  # Handle missing values
    
    atable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range"),
                         Values = c(data["Min."], data["1st Qu."], mean(df()$value, na.rm = TRUE), data["Median"], data["3rd Qu."], data["Max."], data_iqr, data_range))
    
    return(atable)
  })
  
  # Histogram for dynamic mean and SD
  output$hist_dynamic <- renderPlot({
    ggplot(data = df_dynamic(), aes(x = value)) +
      geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 45) +
      geom_density(color = "black", alpha = 0.5, size = 1.5) +
      labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "none")
  })
  
  # Boxplot for dynamic mean and SD
  output$boxplot_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
