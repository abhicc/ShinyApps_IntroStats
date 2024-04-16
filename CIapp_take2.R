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
                 numericInput("param_value", "Population mean (μ):", value = .5, min = 0, max = 1, step = 0.1),
                 numericInput("sd", "Population SD (σ):", value = 10),
                 numericInput("sample_size", "Sample size (n):", value = 50, min = 1),
                 numericInput("num_intervals", "Number of intervals:", value = 10, min = 1),
                 selectInput("confidence", "Confidence level:",
                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99)),
                 textOutput("intervals_containing_param") # Display number of intervals containing the parameter and percentage
               ),
               mainPanel(
                 plotlyOutput("conf_plot") # Plotly plot for displaying confidence intervals for mean
               )
             )),
    tabPanel("Proportion", value = "proportion",
             sidebarLayout(
               sidebarPanel(
                 numericInput("param_value_prop", "Population proportion (π):", value = 0.5, min = 0, max = 1, step = 0.1),
                 numericInput("sample_size_prop", "Sample size (n):", value = 50, min = 1),
                 numericInput("num_intervals_prop", "Number of intervals:", value = 10, min = 1),
                 selectInput("confidence_prop", "Confidence level:",
                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99)),
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
  intervals_containing_param <- function(param_value, sample_size, num_intervals, confidence, param_type) {
    # Generate data for specified number of intervals
    sample_data <- lapply(1:num_intervals, function(i) {
      if (param_type == "mean") {
        data.frame(y = i, x = rnorm(sample_size, mean = param_value, sd = input$sd)) # Normal distribution for mean
      } else if (param_type == "proportion") {
        data.frame(y = i, x = rbinom(sample_size, 1, param_value)) # Binomial distribution for proportion
      }
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(confidence)) / 2, mean = 0, sd = 1)
      if (param_type == "mean") {
        se <- sd(data$x) / sqrt(sample_size) # Correct calculation of standard error
      } else if (param_type == "proportion") {
        p_hat <- mean(data$x) # Sample proportion
        se <- sqrt(p_hat * (1 - p_hat) / sample_size) # Standard error of the proportion
      }
      lower_bound <- mean(data$x) - ci * se
      upper_bound <- mean(data$x) + ci * se
      contains_param <- param_value >= lower_bound & param_value <= upper_bound
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_param = contains_param)
    })
    
    intervals_with_param <- sum(sapply(ci_data, function(ci) {
      if (param_value >= min(ci$xmin, ci$xmax) && param_value <= max(ci$xmin, ci$xmax))  {
        1
      } else {
        0
      }
    }))
    
    intervals_with_param
  }
  
  # Display number of intervals containing the parameter and percentage for mean
  output$intervals_containing_param <- renderText({
    total_intervals <- input$num_intervals
    intervals_with_param <- intervals_containing_param(input$param_value, input$sample_size, input$num_intervals, as.numeric(input$confidence), "mean")
    
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
    intervals_with_param <- intervals_containing_param(input$param_value_prop, input$sample_size_prop, input$num_intervals_prop, as.numeric(input$confidence_prop), "proportion")
    percentage <- round(intervals_with_param / total_intervals * 100, 2)
    paste("Number of intervals containing the parameter:", intervals_with_param, "/", total_intervals, "=", percentage, "%")
  })
  
  # Render the plot for mean simulation
  output$conf_plot <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:input$num_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$sample_size, mean = input$param_value, sd = input$sd)) # Normal distribution for mean
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$sd / sqrt(input$sample_size)) # Standard deviation of the normal distribution is 1
      upper_bound <- mean(data$x) + ci * (input$sd / sqrt(input$sample_size))
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
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#882255"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
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
      ci <- qnorm((1 - as.numeric(input$confidence_prop)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / input$sample_size_prop) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_param <- ifelse(input$param_value_prop >= min(lower_bound, upper_bound) && input$param_value_prop <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_param = contains_param, text = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Create plot for proportion simulation
    gg <- ggplot() +
      geom_vline(xintercept = input$param_value_prop, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax, color = contains_param), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_param, text = text)) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#882255"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
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
