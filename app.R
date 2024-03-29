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
                      sliderInput(inputId = "mean_value",
                                  label = "Input a mean value",
                                  min = -100,
                                  max = 100,
                                  value = 0,
                                  step = 0.1)),
               column(3,
                      sliderInput(inputId = "sd_value",
                                  label = "Input a standard deviation",
                                  min = 1,
                                  max = 15,
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
               column(4, 
                      plotOutput("hist_dynamic")),
               column(4,
                      plotOutput("boxplot_dynamic")),
               column(4, 
                      tableOutput("table_dynamic"))
             )
    )
  )
)



# server function
server <- function(input, output) {
  
  df <- reactive({
    if(input$shape == "Symmetric") {
      val <- rnorm(1000, mean = input$mean_value, sd = input$sd_value)
      df <- data.frame(value = val)
    } else if (input$shape == "Positively Skewed") {
      # Generate 900 normally distributed values and 100 values between the mean and 100 points larger than the mean
      val_right <- c(rnorm(900, mean = input$mean_value, sd = input$sd_value),
                     runif(100, min = input$mean_value, max = input$mean_value+100))
      df <- data.frame(value = val_right)
    } else {
      # Generate 900 normally distributed values and 100 values between the 100 minus the mean and the mean
      
      val_left <- c(rnorm(900, mean = input$mean_value, sd = input$sd_value),
                    runif(100, min = input$mean_value-100, max = input$mean_value))
      df <- data.frame(value = val_left)
    }
    
    # val_right <- c(val, tail(val[order(val)], 100) + rnorm(100, mean=input$mean_value+10, sd=2))
    # val_transform <- log10(val); df <- data.frame(val_transform)
    return(df)
  })
  
  
output$hist <- renderPlot({
    
    ggplot(data = df()) +
      geom_histogram(mapping = aes(x = value, fill = "Histogram"), color = "black", bins = 100) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean"))+
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
      geom_vline(mapping = aes(xintercept = mean(df()$value), color = "Mean"))+
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
################################################################################