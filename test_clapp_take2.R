library(tidyverse)
library(plotly)

# Calculate number of intervals containing μ
  intervals_containing_mu <- reactive({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data_mean <- lapply(1:100, function(i) {
      data.frame(y = i, x = rnorm(100, mean = 0.5, sd = 10)) # normal distribution!
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data_mean, function(data) {
      ci <- qnorm((1 - as.numeric(95)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (10 / sqrt(100))
      upper_bound <- mean(data$x) + ci * (10 / sqrt(100))
      contains_mean <- 0.5 >= min(lower_bound, upper_bound) && 0.5 <= max(lower_bound, upper_bound)
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_mean = contains_mean, bounds_label = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Calculate number of intervals containing μ
    sum(sapply(ci_data, function(ci) {
      if (0.5 >= min(ci$xmin, ci$xmax) && 0.5 <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
  })
  
  # Display number of intervals containing μ and percentage
  output$intervals_containing_mu <- renderText({
    total_intervals <- 100
    intervals_with_mu <- intervals_containing_mu()
    percentage <- round(intervals_with_mu / total_intervals * 100, 2)
    paste("Number of intervals containing μ:", intervals_with_mu, "/", total_intervals, "=", percentage, "%")
  })
  
  
  # Calculate number of intervals containing the parameter (proportion)
  intervals_containing_param_prop <- reactive({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:100, function(i) {
      data.frame(y = i, x = rbinom(50, 1, 0.5)) # Binomial distribution for proportion
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(95)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / 50) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_prop <- ifelse(0.5 >= min(lower_bound, upper_bound) && 0.5 <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_prop = contains_prop, text = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Calculate number of intervals containing the parameter
    sum(sapply(ci_data, function(ci) {
      if (0.5 >= min(ci$xmin, ci$xmax) && 0.5 <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
  })
  
  # Display number of intervals containing the parameter and percentage for proportion
  output$intervals_containing_param_prop <- renderText({
    total_intervals <- 100
    intervals_with_param <- intervals_containing_param_prop()
    percentage <- round(intervals_with_param / total_intervals * 100, 2)
    paste("Number of intervals containing π:", intervals_with_param, "/", total_intervals, "=", percentage, "%")
  })
  
  # Render the plot for mean simulation
  output$conf_plot <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data_mean <- lapply(1:100, function(i) {
      data.frame(y = i, x = rnorm(50, mean = 0.5, sd = 10)) # Normal distribution for mean
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data_mean, function(data) {
      ci <- qnorm((1 - as.numeric(95)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (10 / sqrt(50)) # Standard deviation of the normal distribution is 1
      upper_bound <- mean(data$x) + ci * (10 / sqrt(50))
      contains_mean <- ifelse(0.5 >= min(lower_bound, upper_bound) && 0.5 <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_mean = contains_mean)
    })
    
    # Create plot for mean simulation
    gg <- ggplot() +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmax, xmax = xmin, color = contains_mean), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_mean, text = paste("Lower bound:", round(xmax, 2), "<br>Upper bound:", round(xmin, 2)))) +
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
    sample_data <- lapply(1:100, function(i) {
      data.frame(y = i, x = rbinom(50, 1, 0.5)) # Binomial distribution for proportion
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(95)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / 50) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_prop <- ifelse(0.5 >= min(lower_bound, upper_bound) && 0.5 <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_prop = contains_prop, text = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Create plot for proportion simulation
    gg <- ggplot() +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax, color = contains_prop), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_prop, text = paste("Lower bound:", round(xmax, 2), "<br>Upper bound:", round(xmin, 2)))) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#882255"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Proportion",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) # Convert ggplot2 figure into an interactive plotly plot
  })