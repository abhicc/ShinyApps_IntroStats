################################################################################
# This app demonstrates the concept of confidence intervals for various contexts,  
# specifically the ideas such as confidence intervals are not fixed, they might not 
# contain the "true" population parameter, how CIs are affected by sample sizes, 
# the interpretation of CIs.
################################################################################

library(plotly)
library(shiny)
# Define UI
ui <- fluidPage(
  titlePanel("Simulating Confidence Intervals"), # Application title 
  tabsetPanel(
    tabPanel("Mean", value = "mean",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("pop_mean", "Population mean (μ):", value = 0.5, min = -10, max = 10, step = 0.1),
                 sliderInput("pop_sd", "Population SD (σ):", value = 10, min = 0, max = 20),
                 sliderInput("sample_size", "Sample size (n):", value = 100, min = 1, max = 100),
                 sliderInput("num_intervals_mean", "Number of intervals:", value = 10, min = 1, max = 100),
                 selectInput("confidence_mean", "Confidence level:",
                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99)),
                 textOutput("intervals_containing_mu"), # Display number of intervals containing the parameter and percentage
                 textOutput("green_lines_count_mean") # Display the count of green lines
               ),
               mainPanel(
                 plotlyOutput("conf_plot_mean") # Plotly plot for displaying confidence intervals for mean
               )
             )),
    tabPanel("Proportion", value = "proportion",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("pop_prop", "Population proportion (π):", value = 0.5, min = 0, max = 1, step = 0.1),
                 sliderInput("sample_size_prop", "Sample size (n):", value = 100, min = 1, max = 100),
                 sliderInput("num_intervals_prop", "Number of intervals:", value = 10, min = 1, max = 100),
                 selectInput("confidence_prop", "Confidence level:",
                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99)),
                 textOutput("intervals_containing_param_prop"), # Display number of intervals containing the parameter and percentage
                 textOutput("green_lines_count_prop") # Display the count of green lines
               ),
               mainPanel(
                 plotlyOutput("conf_plot_prop") # Plotly plot for displaying confidence intervals for proportion
               )
             ))
  ),

    fluidRow(
    column(
      width = 12,
      p("App created by Nina Austria and Andrea Zantek and maintained by Abhishek Chakraborty", align = "center", style = "margin-top: 20px;")
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for calculating number of intervals containing μ
  intervals_containing_mu <- reactiveVal(0)
  
  # Watch for changes in relevant inputs and update the reactive expression
  observe({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data_mean <- lapply(1:input$num_intervals_mean, function(i) {
      data.frame(y = i, x = rnorm(input$sample_size, mean = input$pop_mean, sd = input$pop_sd)) # normal distribution!
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data_mean, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence_mean)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$pop_sd / sqrt(input$sample_size))
      upper_bound <- mean(data$x) + ci * (input$pop_sd / sqrt(input$sample_size))
      contains_mean <- input$pop_mean >= min(lower_bound, upper_bound) && input$pop_mean <= max(lower_bound, upper_bound)
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_mean = contains_mean, bounds_label = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Calculate number of intervals containing μ
    intervals_with_mu <- sum(sapply(ci_data, function(ci) {
      if (input$pop_mean >= min(ci$xmin, ci$xmax) && input$pop_mean <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
    
    intervals_containing_mu(intervals_with_mu)
  })
  
  # Display number of intervals containing μ and percentage
  # output$intervals_containing_mu <- renderText({
  #   total_intervals <- input$num_intervals_mean
  #   intervals_with_mu <- intervals_containing_mu()
  #   percentage <- round(intervals_with_mu / total_intervals * 100, 2)
  #   paste("Number of intervals containing μ:", intervals_with_mu, "/", total_intervals, "=", percentage, "%")
  # })
  # 
  # Render the plot for mean simulation
  output$conf_plot_mean <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data_mean <- lapply(1:input$num_intervals_mean, function(i) {
      data.frame(y = i, x = rnorm(input$sample_size, mean = input$pop_mean, sd = input$pop_sd)) # Normal distribution for mean
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data_mean, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence_mean)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$pop_sd / sqrt(input$sample_size)) # Standard deviation of the normal distribution is 1
      upper_bound <- mean(data$x) + ci * (input$pop_sd / sqrt(input$sample_size))      
      
      contains_mean <- ifelse(input$pop_mean >= min(lower_bound, upper_bound) && input$pop_mean <= max(lower_bound, upper_bound), "Contains Parameter", "Doesn't Contain Parameter")
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_mean = contains_mean)
    })
    
    # Count the number of green lines
    num_green_lines <- sum(sapply(ci_data, function(ci) {
      if (ci$contains_mean[1] == "Contains Parameter") {
        1
      } else {
        0
      }
    }))
    
    # Create plot for mean simulation
    gg <- ggplot() +
      geom_vline(xintercept = input$pop_mean, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmax, xmax = xmin, color = contains_mean), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 #   mapping = aes(y = y, x = x, color = contains_mean, text = paste("Contains Parameter:", contains_mean, "<br>Lower bound:", round(xmax, 2), "<br>Upper bound:", round(xmin, 2))), 
                 mapping = aes(y = y, x = x, color = contains_mean, 
                               text = paste("Contains Parameter:", ifelse(contains_mean == "Contains Parameter", "TRUE", "FALSE"), "<br>Lower bound:", round(xmax, 2), "<br>Upper bound:", round(xmin, 2))), 
                 show.legend = FALSE) +
      
      scale_color_manual(values = c("Contains Parameter" = "#009E73", "Doesn't Contain Parameter" = "#882255"), guide = FALSE) +
      labs(title = "Confidence Intervals",
           x = "Mean",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) +  # Hide y-axis label 
      geom_text(aes(x = Inf, y = Inf, label = factor(c("TRUE", "FALSE"))), 
                hjust = 1.1, vjust = c(1.1, -0.2), size = 3, show.legend = TRUE)
    
    output$green_lines_count_mean <- renderText({
      percentage <- round(num_green_lines / input$num_intervals_mean * 100, 2)
      paste("Percentage of intervals containing μ:",  num_green_lines, "/", input$num_intervals_mean, "=", percentage, "%")
    })
    
    gg <- ggplotly(gg, tooltip = "text")
    
    gg <- ggplotly(gg) %>%
      layout(legend = list(
        itemsizing = "constant",
        orientation = "v",
        traceorder = "normal",
        title = list(text = "         Click to Hide:"),  # Add a title to the legend
        labels = list(
          "Contains Parameter" = "Contains Parameter",
          "Doesn't Contain Parameter" = "Doesn't Contain Parameter"  # Update label to reflect "μ"
        )      ))
    
    
    gg
  })
  
  # Reactive expression for calculating number of intervals containing the parameter (proportion)
  intervals_containing_param_prop <- reactiveVal(0)
  
  # Watch for changes in relevant inputs and update the reactive expression
  observe({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:input$num_intervals_prop, function(i) {
      data.frame(y = i, x = rbinom(input$sample_size_prop, 1, input$pop_prop)) # Binomial distribution for proportion
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence_prop)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / input$sample_size_prop) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_prop <- ifelse(input$pop_prop >= min(lower_bound, upper_bound) && input$pop_prop <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_prop = contains_prop, text = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Calculate number of intervals containing the parameter
    intervals_with_param <- sum(sapply(ci_data, function(ci) {
      if (input$pop_prop >= min(ci$xmin, ci$xmax) && input$pop_prop <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
    
    intervals_containing_param_prop(intervals_with_param)
  })
  
  # Display number of intervals containing the parameter and percentage for proportion
  # output$intervals_containing_param_prop <- renderText({
  #   total_intervals <- input$num_intervals_prop
  #   intervals_with_param <- intervals_containing_param_prop()
  #   percentage <- round(intervals_with_param / total_intervals * 100, 2)
  #   paste("Number of intervals containing π:", intervals_with_param, "/", total_intervals, "=", percentage, "%")
  # })
  
  # Render the plot for proportion simulation
  output$conf_plot_prop <- renderPlotly({
    # Generate data for specified number of intervals
    sample_data <- lapply(1:input$num_intervals_prop, function(i) {
      data.frame(y = i, x = rbinom(input$sample_size_prop, 1, input$pop_prop)) # Binomial distribution for proportion
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence_prop)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x) # Sample proportion
      se <- sqrt(p_hat * (1 - p_hat) / input$sample_size_prop) # Standard error of the proportion
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_prop <- ifelse(input$pop_prop >= min(lower_bound, upper_bound) && input$pop_prop <= max(lower_bound, upper_bound), "Contains Parameter", "Doesn't Contain Parameter")
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_prop = contains_prop, text = paste("Lower bound:", round(lower_bound, 2), "<br>Upper bound:", round(upper_bound, 2)))
    })
    
    # Count the number of green lines
    num_green_lines <- sum(sapply(ci_data, function(ci) {
      if (ci$contains_prop[1] == "Contains Parameter") {
        1
      } else {
        0
      }
    }))
    
    # Create plot for proportion simulation
    gg <- ggplot() +
      geom_vline(xintercept = input$pop_prop, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax, color = contains_prop), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x, color = contains_prop, text = paste("Contains Parameter:", ifelse(contains_prop == "Contains Parameter", "TRUE", "FALSE"), "<br>Lower bound:", round(xmax, 2), "<br>Upper bound:", round(xmin, 2))), 
                 show.legend = FALSE) +
      scale_color_manual(values = c("Contains Parameter" = "#009E73", "Doesn't Contain Parameter" = "#882255"), guide = FALSE, labels = c("Contains Parameter" = "Contains Parameter", "Doesn't Contain Parameter" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Proportion",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    # Update the count of green lines
    output$green_lines_count_prop <- renderText({
      percentage <- round(num_green_lines / input$num_intervals_prop * 100, 2)
      paste("Percentage of intervals containing π:",  num_green_lines, "/", input$num_intervals_prop, "=", percentage, "%")
    })
    
    gg <- ggplotly(gg, tooltip = "text")
    
    gg <- ggplotly(gg) %>%
      layout(legend = list(
        itemsizing = "constant",
        orientation = "v",
        traceorder = "normal",
        title = list(text = "         Click to Hide:"),  # Add a title to the legend
        labels = list(
          "Contains Parameter" = "Contains Parameter",
          "Doesn't Contain Parameter" = "Doesn't Contain Parameter"  # Update label to reflect "μ"
        )      ))
    
    
    gg
  })
}
# Run the application
shinyApp(ui = ui, server = server)