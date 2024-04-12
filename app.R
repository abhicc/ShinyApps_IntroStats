
################################################################################
# This app demonstrates the concept of skewness (symmetricity) in terms of 
# histograms and boxplots. It also demonstrates how the mean and standard deviation
# affects these plots, and how numerical summaries are affected by extreme outliers.
################################################################################

HappyPlanetIndex <- read.csv("HappyPlanetIndex.csv")
RestaurantTips <- read.csv("RestaurantTips.csv")
FloridaLakes <- read.csv("FloridaLakes.csv")
SleepStudy <- read.csv("SleepStudy.csv")
StudentSurvey <- read.csv("StudentSurvey.csv")
USStates <- read.csv("USStates1e.csv")
HomesForSale <- read.csv("HomesForSale.csv")

library(sn)
library(tidyverse)
library(patchwork)
library(mosaic)
library(fGarch)
library(shiny)

# UI code
# UI code

ui <- fluidPage(
  titlePanel("Summarizing Numerical Data: Visualizing Mean and Median"),
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
             )
    ),
    
    tabPanel("Dynamic Mean and SD",
             fluidRow(
               column(3, sliderInput(inputId = "mean_value_dynamic",
                                     label = "Input a mean value",
                                     min = -100,
                                     max = 100,
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
             )
    ),
    
    tabPanel("Upload File",
             fluidRow(
               column(4, 
                      sidebarPanel(
                        h4("Choose Dataset or Upload a CSV file"),
                        radioButtons("data_choice", "Select Data Source:", 
                                     choices = c( "Upload CSV File", "Preloaded Dataset"),
                                     selected = "Upload CSV File"),
                        conditionalPanel(
                          condition = "input.data_choice == 'Upload CSV File'",
                          fileInput("file", "", accept = ".csv")
                        ),
                        uiOutput("dataset_select"),
                        uiOutput("variable_select")
                      )),
               column(8, 
                      uiOutput("data_plot"),
                      tableOutput("data_table")
               )
             )
    )
  )
)

# server function
server <- function(input, output, session) {
  
  output$dataset_select <- renderUI({
    if (input$data_choice == "Preloaded Dataset") {
      return(selectInput("dataset", "Choose a Dataset", choices = c("HappyPlanetIndex", "USStates", "SleepStudy", "StudentSurvey", "RestaurantTips","HomesForSale", "FloridaLakes" ), selected = "HappyPlanetIndex"))
    } else {
      return(NULL)
    }
  })
  
  output$variable_select <- renderUI({
    if (input$data_choice == "Preloaded Dataset") {
      if (!is.null(input$dataset)) {
        return(selectInput("var", "Choose a Variable", choices = names(get(input$dataset))))
      } else {
        return(NULL)
      }
    } else {
      if (!is.null(input$file)) {
        df <- read.csv(input$file$datapath)
        return(selectInput("var", "Choose a Variable", choices = names(df)))
      } else {
        return(NULL)
      }
    }
  })
  
  output$data_plot <- renderUI({
    if (input$data_choice == "Preloaded Dataset") {
      plotOutput("dataset_plot", width = "100%", height = "400px")
    } else {
      plotOutput("upload_plot", width = "100%", height = "400px")
    }
  })
  
  output$data_table <- renderTable({
    if (input$data_choice == "Preloaded Dataset") {
      req(input$var, input$data_choice, input$dataset)
      data <- get(input$dataset)
      if (!is.null(data)) {
        if (input$var %in% names(data)) {
          data_column <- data[[input$var]]
          data_summary <- summary(data_column, na.rm = TRUE)
          data_iqr <- IQR(data_column, na.rm = TRUE)
          data_range <- diff(range(data_column, na.rm = TRUE))
          missing_count <- sum(is.na(data_column))
          atable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range","Number of NA Observations"),
                               Values = c(data_summary["Min."], data_summary["1st Qu."], mean(data_column), data_summary["Median"],
                                          data_summary["3rd Qu."], data_summary["Max."],
                                          data_iqr,
                                          data_range,
                                          missing_count))
          return(atable)
        }
      }
    } else {
      req(input$var, input$file$datapath)
      df <- read.csv(input$file$datapath)
      data_column <- df[[input$var]]
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
  })
  
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE)
    updateSelectInput(session, "var", choices = names(df))
  })
  
  output$dataset_plot <- renderPlot({
    req(input$var, input$data_choice, input$dataset)
    data <- get(input$dataset)
    if (!is.null(data)) {
      if (input$var %in% names(data)) {
        data_column <- data[[input$var]]
        data <- data[complete.cases(data_column) & is.finite(data_column), ]
        if (is.numeric(data_column)) {
         
          
          
          
          p <-  ggplot(data = data, aes_string(x = input$var)) +
            geom_histogram(mapping = aes(fill = "Histogram"), color = "black", bins = 30) +
            labs(x = "Value", y = "Frequency", title = "Histogram") +
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
          gridExtra::grid.arrange(p, bp, nrow = 1)
          
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  output$upload_plot <- renderPlot({
    req(input$var, input$file$datapath)
    df <- read.csv(input$file$datapath)
    if (is.numeric(df[[input$var]])) {
      data_column <- df[[input$var]]
      data <- df[complete.cases(data_column) & is.finite(data_column), ]
      p <- ggplot(data = data, aes_string(x = input$var)) +
        geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 30) +
        geom_density(color = "black", alpha = 0.5, size = 1.5) +
        labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
        geom_vline(mapping = aes(xintercept = mean(data_column), color = "Mean"), size = 2) +
        geom_vline(mapping = aes(xintercept = median(data_column), color = "Median"), size = 2) +
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
      gridExtra::grid.arrange(p, bp, nrow = 1)
    } else {
      return(NULL)
    }
  })

  
  
  
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
  
  output$table <- renderTable({
    data <- summary(df()$value)
    data_iqr <- IQR(df()$value, na.rm = TRUE)  # Handle missing values
    data_range <- diff(range(df()$value, na.rm = TRUE))  # Handle missing values
    
    atable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range"),
                         Values = c(data["Min."], data["1st Qu."], mean(df()$value, na.rm = TRUE), data["Median"], data["3rd Qu."], data["Max."], data_iqr, data_range))
    
    return(atable)
  })
  
  
  output$hist_dynamic <- renderPlot({
    ggplot(data = df_dynamic(), aes(x = value)) +
      geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 45) +
      geom_density(color = "black", alpha = 0.5, size = 1.5) +
      labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      theme(legend.position = "none")
  })
  
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

shinyApp(ui = ui, server = server)