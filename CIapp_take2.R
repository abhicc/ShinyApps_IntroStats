################################################################################
# This app demonstrates the concept of confidence intervals for various contexts,  
# specifically the ideas such as confidence intervals are not fixed, they might not 
# contain the "true" population parameter, how CIs are affected by sample sizes, 
# the interpretation of CIs.
################################################################################
library(tidyverse)
library(plotly)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Simulating Confidence Intervals"), # Application title 
  tabsetPanel(
    tabPanel("Mean", value = "mean",
             sidebarLayout(
               sidebarPanel(
                 numericInput("param_value", "Parameter value:", value = .5, min = 0, max = 1, step = 0.1),
                 numericInput("sample_size", "Sample size (n):", value = 50, min = 1),
                 numericInput("num_intervals", "Number of intervals:", value = 10, min = 1),
                 sliderInput("confidence", "Confidence Level:",
                             min = 0.01, max = 0.99, value = 0.95, step = 0.01),
                 textOutput("intervals_containing_param") # Display number of intervals containing the parameter and percentage
               ),
               mainPanel(
                 plotlyOutput("conf_plot") # Plotly plot for displaying confidence intervals for mean
               )
             )),
    tabPanel("Proportion", value = "proportion",
             sidebarLayout(
               sidebarPanel(
                 numericInput("param_value_prop", "Parameter value:", value = 0.5, min = 0, max = 1, step = 0.1),
                 numericInput("sample_size_prop", "Sample size (n):", value = 50, min = 1),
                 numericInput("num_intervals_prop", "Number of intervals:", value = 10, min = 1),
                 sliderInput("confidence_prop", "Confidence Level:",
                             min = 0.01, max = 0.99, value = 0.95, step = 0.01),
                 textOutput("intervals_containing_param_prop") # Display number of intervals containing the parameter and percentage
               ),
               mainPanel(
                 plotlyOutput("conf_plot_prop") # Plotly plot for displaying confidence intervals for proportion
               )
             ))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to calculate number of intervals containing the parameter (mean or proportion)
  intervals_containing_param <- function(param_value, sample_size, num_intervals, confidence) {
    # Generate data for specified number of intervals
    sample_data <- lapply(1:num_intervals, function(i) {
      data.frame(y = i, x = rnorm(sample_size, mean = param_value, sd = 1)) # Normal distribution for mean
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - confidence) / 2, mean = 0, sd = 1)
      se <- sd(data$x) / sqrt(sample_size) # Correct calculation of standard error
      lower_bound <- mean(data$x) - ci * se
      upper_bound <- mean(data$x) + ci * se
      contains_param <- param_value >= lower_bound & param_value <= upper_bound
      print(paste("Parameter Value:", param_value))
      print(paste("Lower Bound:", lower_bound))
      print(paste("Upper Bound:", upper_bound))
      print(paste("Contains Param:", contains_param))
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_param = contains_param)
    })
    
    intervals_with_param <- sum(sapply(ci_data, function(ci) {
      if (any(ci$contains_param)) {
        1
      } else {
        0
      }
    }))
    
    print(paste("Intervals with parameter:", intervals_with_param))
    
    intervals_with_param
  }
  
  # Display number of intervals containing the parameter and percentage for mean
  output$intervals_containing_param <- renderText({
    total_intervals <- input$num_intervals
    intervals_with_param <- intervals_containing_param(input$param_value, input$sample_size, input$num_intervals, input$confidence)
    
    if (intervals_with_param > 0) {
      percentage <- round(intervals_with_param / total_intervals * 100, 2)
      result <- paste("Number of intervals containing the parameter:", intervals_with_param, "/", total_intervals, "=", percentage, "%")
    } else {
      result <- "None of the intervals contain the parameter."
    }
    
    return(result)
  })
  
  
  
  # Display number of intervals containing the parameter and percentage for proportion
  output$intervals_containing_param_prop <- renderText({
    total_intervals <- input$num_intervals_prop
    intervals_with_param <- intervals_containing_param(input$param_value_prop, input$sample_size_prop, input$num_intervals_prop, input$confidence_prop)
    percentage <- round(intervals_with_param / total_intervals * 100, 2)
    paste("Number of intervals containing the parameter:", intervals_with_param, "/", total_intervals, "=", percentage, "%")
  })
  
  # Render the plot for mean simulation
  output$conf_plot <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:input$num_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$sample_size, mean = input$param_value, sd = 1)) # Normal distribution for mean
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (1 / sqrt(input$sample_size)) # Standard deviation of the normal distribution is 1
      upper_bound <- mean(data$x) + ci * (1 / sqrt(input$sample_size))
      contains_param <- ifelse(input$param_value >= min(lower_bound, upper_bound) && input$param_value <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_param = contains_param)
    })
    
    # Create plot for mean simulation
    gg <- ggplot() +
      geom_vline(xintercept = input$param_value, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax, color = contains_param), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_param)) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#D55E00"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Mean",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) # Convert ggplot2 figure into an interactive plotly plot
  })
  
  # Render the plot for proportion simulation
  output$conf_plot_prop <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:input$num_intervals_prop, function(i) {
      data.frame(y = i, x = rbinom(input$sample_size_prop, 1, input$param_value_prop)) # Binomial distribution for proportion
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence_prop) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / input$sample_size_prop) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_param <- ifelse(input$param_value_prop >= min(lower_bound, upper_bound) && input$param_value_prop <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_param = contains_param)
    })
    
    # Create plot for proportion simulation
    gg <- ggplot() +
      geom_vline(xintercept = input$param_value_prop, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax, color = contains_param), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_param)) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#D55E00"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Proportion",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) # Convert ggplot2 figure into an interactive plotly plot
  })
}

shinyApp(ui = ui, server = server)