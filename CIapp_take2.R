################################################################################
# This app demonstrates the concept of confidence intervals for various contexts,  
# specifically the ideas such as confidence intervals are not fixed, they might not 
# contain the "true" population parameter, how CIs are affected by sample sizes, 
# the interpretation of CIs.
################################################################################
# Required libraries
library(tidyverse)
library(mosaic)
library(shiny)
################################################################################

# Define UI
ui <- fluidPage(
  titlePanel("Simulating Confidence Intervals"), # Application title 
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", "Population mean (μ):", value = 0.5, step = 0.1),
      numericInput("sd", "Population standard deviation (σ):", value = 10),
      numericInput("n", "Sample size (n):", value = 50),
      numericInput("n_intervals", "Number of intervals:", value = 10),
      sliderInput("confidence", "Confidence Level:",
                  min = 0.01, max = 0.99, value = 0.95, step = 0.01),
      textOutput("intervals_containing_mu") # display number of intervals containing μ and percentage
    ),
    mainPanel(
      plotlyOutput("conf_plot") # plotly allows for plots with interactive features 
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculate number of intervals containing μ
  intervals_containing_mu <- reactive({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data <- lapply(1:input$n_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$n, mean = input$mean, sd = input$sd)) # normal distribution!
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$sd / sqrt(input$n))
      upper_bound <- mean(data$x) + ci * (input$sd / sqrt(input$n))
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound)
    })
    
    # Calculate number of intervals containing μ
    sum(sapply(ci_data, function(ci) {
      if (input$mean >= min(ci$xmin, ci$xmax) && input$mean <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
  })
  
  # Display number of intervals containing μ and percentage
  output$intervals_containing_mu <- renderText({
    total_intervals <- input$n_intervals
    intervals_with_mu <- intervals_containing_mu()
    percentage <- round(intervals_with_mu / total_intervals * 100, 2)
    paste("Number of intervals containing μ:", intervals_with_mu, "/", total_intervals, "=", percentage, "%")
  })
  
  output$conf_plot <- renderPlotly({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data <- lapply(1:input$n_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$n, mean = input$mean, sd = input$sd)) # normal distribution
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$sd / sqrt(input$n))
      upper_bound <- mean(data$x) + ci * (input$sd / sqrt(input$n))
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound)
    })
    
    # Create plot
    gg <- ggplot() +
      geom_vline(xintercept = input$mean, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax), 
                     color = "#009E73", height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x), color = "#009E73") +
      labs(title = "Confidence Intervals",
           x = "Population Mean",
           y = "Interval") +
      theme_minimal()
    
    ggplotly(gg) # for converting ggplot2 figures into interactive ones powered by plotly (using js code)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
