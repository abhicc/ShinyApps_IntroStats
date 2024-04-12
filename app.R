################################################################################
# This app demonstrates the concept of skewness (symmetricity) in terms of 
# histograms and boxplots. It also demonstrates how the mean and standard deviation
# affects these plots, and how numerical summaries are affected by extreme outliers.
################################################################################
# Required libraries
library(sn)
library(tidyverse)
library(patchwork)
library(mosaic)
library(fGarch)
library(shiny)


HomesForSale <- read.csv("HomesForSale.csv")
HappyPlanetIndex <- read.csv("HappyPlanetIndex.csv")
USStates1e <- read.csv("USStates1e.csv")
StudentSurvey <- read.csv("StudentSurvey.csv")
FloridaLakes <- read.csv("FloridaLakes.csv")
RestaurantTips <- read.csv("RestaurantTips.csv")
SleepStudy <- read.csv("SleepStudy.csv")

# user interface

ui <- fluidPage(
  # Application title
  titlePanel("Summarizing Numerical Data: Visualizing Mean and Median"),
  
  # Tabset
  tabsetPanel(
    # Fixed Mean and SD tab
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
    
    # Dynamic Mean and SD tab
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
    
    # Upload File tab
    tabPanel("Upload File",
             fluidRow(
               column(6, h4("Choose Dataset or Upload a CSV file")),
               fluidRow(
                 column(6, 
                        radioButtons("data_choice", "Select Data Source:", 
                                     choices = c("Preloaded Dataset", "Upload CSV File"),
                                     selected = "Preloaded Dataset")),
                 column(6, 
                        selectInput("dataset", "Choose a Dataset", choices = c("mtcars", "iris", 
                                                                               "HomesForSale", "HappyPlanetIndex", 
                                                                               "USStates1e", "StudentSurvey", 
                                                                               "FloridaLakes", "RestaurantTips", 
                                                                               "SleepStudy"), selected = "mtcars"))
               ),
               fluidRow(
                 column(6, 
                        fileInput("file", "", accept = ".csv")),
                 column(6, 
                        selectInput("var", "", ""))
               ),
               fluidRow(
                 column(6, 
                        plotOutput("uploaded_plot", width = "100%", height = "400px")),
                 column(6, 
                        tableOutput("uploaded_table"))
               )
             ),
             # Conditional rendering to hide/show the upload file button
             uiOutput("upload_button_visibility")
    )
  )
)

# server function
server <- function(input, output, session) {
  
  # Reactive function to get the selected data
  data_selected <- reactive({
    if (input$data_choice == "Preloaded Dataset") {
      return(get(input$dataset))
    } else {
      req(input$file)
      return(read.csv(input$file$datapath))
    }
  })
  
  # Update select input choices when a preloaded dataset is selected
  observeEvent(input$data_choice, {
    if (input$data_choice == "Preloaded Dataset") {
      updateSelectInput(session, "dataset", choices = c("mtcars", "iris"))
    } else {
      updateSelectInput(session, "dataset", choices = NULL)
    }
  }, ignoreInit = TRUE)
  
  # Update select input choices when a dataset is selected
  observeEvent(input$dataset, {
    req(input$dataset)
    updateSelectInput(session, "var", choices = names(data_selected()))
  }, ignoreInit = TRUE)
  
  # Reactive function for uploaded data
  uploaded_data <- reactive({
    req(data_selected())
    df <- data_selected()
    if (!is.null(df)) {
      updateSelectInput(session, "var", choices = names(df))
    }
    return(df)
  })
  
  output$uploaded_plot <- renderPlot({
    req(uploaded_data(), input$var)
    
    data <- uploaded_data()
    
    if (is.numeric(data[[input$var]])) {
      # Quantitative variable selected
      data <- uploaded_data()
      
      # Filter out missing and non-finite values
      data <- data[complete.cases(data[[input$var]]) & is.finite(data[[input$var]]), ]
      
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
      
    } else if (!is.numeric(data[[input$var]]) || (is.numeric(data[[input$var]]) && length(unique(data[[input$var]])) < 10)) {
      # Qualitative variable selected
      ggplot(data = data, aes_string(x = input$var)) +
        geom_bar(fill = "#56B4E9", color = "black") +
        labs(x = "Value", y = "Frequency", title = "Bar Plot") +
        theme(legend.position = "none")
    } else {
      # Non-numeric variable selected
      ggplot() +
        geom_point() +  # Placeholder plot to prevent the error
        labs(title = "Invalid Variable Type Selected")
    }
  })
  
  output$uploaded_table <- renderTable({
    req(uploaded_data(), input$var)
    data <- uploaded_data()
    if (!is.null(data)) {
      if (is.numeric(data[[input$var]])) {
        # Quantitative variable selected
        data_column <- data[[input$var]]
        data_summary <- summary(data_column, na.rm = TRUE)
        data_iqr <- IQR(data_column, na.rm = TRUE)
        data_range <- diff(range(data_column, na.rm = TRUE))
        
        missing_count <- sum(is.na(data_column))
        
        atable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range","Number of NA Observations"),
                             Values = c(data_summary["Min."], data_summary["1st Qu."], data_summary["Mean"], data_summary["Median"],
                                        data_summary["3rd Qu."], data_summary["Max."],
                                        data_iqr,
                                        data_range,
                                        missing_count))
        
        return(atable)
      } else if (!is.numeric(data[[input$var]])) {
        # Qualitative variable selected
        frequency_table <- table(data[[input$var]])
        frequency_df <- as.data.frame(frequency_table)
        names(frequency_df) <- c("Category", "Frequency")
        return(frequency_df)
      } else {
        # Non-numeric variable selected
        return(NULL)
      }
    }
  })
  
  observeEvent(input$file, {
    req(uploaded_data())
    updateSelectInput(session, "var", choices = names(uploaded_data()))
  })
  
  # Conditional rendering for hiding/showing upload button
  output$upload_button_visibility <- renderUI({
    if (input$data_choice == "Upload CSV File") {
      return(NULL) # Hide the upload button
    } else {
      return(tags$style("#file { display: none; }")) # Hide the file input
    }
  })
  
  set.seed(422024)
  # Create dataframe for fixed tab
  df <- reactive({
    if(input$shape == "Symmetric") {
      # Generate 1000 random #s from a normal dist with fixed mean 0 and sd 10 
      val <- rnorm(1000, mean = 0, sd = 10) 
      df <- data.frame(value = val) # Create df with one col named value, containing the generated random numbers
    } else if (input$shape == "Positively Skewed") {
      # Generate 900 random #s from a normal dist with mean 0 and sd 10 then append 600 uniform random #s (runif) between 0 and 100 to create positive skewness
      val_right <- c(rnorm(900, mean = 0, sd = 10), 
                     runif(500, min = 0, max = 25),
                     runif(400, min = 25, max = 50),
                     runif(50, min = 50, max = 75),
                     runif(300, min = 50, max = 100))
      df <- data.frame(value = val_right)
    } else {
      # Generate 900 random #s from a normal dist with mean 0 and sd 10 then append 600 uniform random #s (runif) between -100 and 0 to create negative skewness
      val_left <- c(rnorm(900, mean = 0, sd = 10),
                    runif(500, min = -25, max = 0), 
                    runif(400, min = -50, max = -25), 
                    runif(50, min = -75, max = -50), 
                    runif(300, min = -100, max = -50))
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
      # Generate 900 random #s from a normal dist with mean and sd determined by user then append 600 uniform random #s (runif) to create positive skewness
    } else if (input$shape_dynamic == "Positively Skewed") {
      
      val_right <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                     runif(500, min = input$mean_value_dynamic, max = input$mean_value_dynamic+25), 
                     runif(400, min = input$mean_value_dynamic+25, max = input$mean_value_dynamic+50), 
                     runif(50, min = input$mean_value_dynamic+50, max = input$mean_value_dynamic+75), 
                     runif(300, min = input$mean_value_dynamic+50, max = input$mean_value_dynamic+100))
      
      df <- data.frame(value = val_right)
      # Generate 900 random #s from a normal dist with mean and sd determined by user then append 600 uniform random #s (runif) to create negative skewness
    } else {
      val_left <- c(rnorm(900, mean = input$mean_value_dynamic, sd = input$sd_value_dynamic),
                    runif(500, min = input$mean_value_dynamic-25, max = input$mean_value_dynamic), 
                    runif(400, min = input$mean_value_dynamic-50, max = input$mean_value_dynamic-25), 
                    runif(50, min = input$mean_value_dynamic-75, max = input$mean_value_dynamic-50), 
                    runif(300, min = input$mean_value_dynamic-100, max = input$mean_value_dynamic-50)) # increasing negative skewness
      df <- data.frame(value = val_left)
    }
    return(df)
  })
  
  output$hist <- renderPlot({
    ggplot(data = df(), aes(x = value)) +
      geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 45) +
      geom_density(color = "black", alpha = 0.5, size = 1.5) + # Add density plot
      labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  
  output$hist_dynamic <- renderPlot({
    ggplot(data = df_dynamic(), aes(x = value)) +
      geom_histogram(mapping = aes(y = ..density.., fill = "Histogram"), color = "black", bins = 45) +
      geom_density(color = "black", alpha = 0.5, size = 1.5) + # Add density plot
      labs(x = "Value", y = "Density", title = "Histogram with Density Plot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean"), size = 2) +
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
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
  
  output$boxplot_dynamic <- renderPlot({
    ggplot(data = df_dynamic()) +
      geom_boxplot(mapping = aes(x = value, fill = "Boxplot"), color = "black") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      geom_vline(mapping = aes(xintercept = mean(df_dynamic()$value), color = "Mean"), size = 2) + # Calculate mean dynamically
      geom_vline(mapping = aes(xintercept = median(df_dynamic()$value), color = "Median"), size = 2) +
      scale_color_manual("", values = c(Mean = "#D55E00", Median = "#882255")) +
      scale_fill_manual("", values = c("#56B4E9"), guide = FALSE) +
      xlim(c(-100, 100)) +
      theme(legend.position = "right")
  })
  
  # Summary Table for Fixed Mean and SD tab
  output$table <- renderTable({
    res <- favstats(~ value, data=df())
    ftable <- data.frame(Summary = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "IQR", "Range"),
                         Values = c(round(res$min,3), round(res$Q1,3), round(res$mean,3), round(res$median,3), 
                                    round(res$Q3,3), round(res$max,3), 
                                    round(res$Q3,3) - round(res$Q1,3),
                                    round(res$max,3) - round(res$min,3)))
    
    ftable
  })
}

# Run the application
shinyApp(ui = ui, server = server)



################################################################################