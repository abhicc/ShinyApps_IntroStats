
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
                 numericInput("pop_mean", "Population mean (μ):", value = 0.5, min = 0, max = 1, step = 0.1),
                 numericInput("pop_sd", "Population SD (σ):", value = 10),
                 numericInput("sample_size", "Sample size (n):", value = 50, min = 1),
                 numericInput("num_intervals", "Number of intervals:", value = 10, min = 1),
                 selectInput("confidence", "Confidence level:",
                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99)),
                 textOutput("intervals_containing_mu") # Display number of intervals containing the parameter and percentage
               ),
               mainPanel(
                 plotlyOutput("conf_plot") # Plotly plot for displaying confidence intervals for mean
               )
             )),
    tabPanel("Proportion", value = "proportion",
             sidebarLayout(
               sidebarPanel(
                 numericInput("pop_prop", "Population proportion (π):", value = 0.5, min = 0, max = 1, step = 0.1),
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
# Define server logic
server <- function(input, output) {
  
  # Initialize ci_data for storing confidence interval data
  ci_data <- reactiveValues()
  
  # Initialize selected intervals
  selected_intervals <- reactiveVal(NULL)
  
  # Generate and store ci_data for mean simulation
  observeEvent(input$num_intervals, {
    sample_data_mean <- lapply(1:input$num_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$sample_size, mean = input$pop_mean, sd = input$pop_sd))
    })
    
    ci_data$mean <- lapply(sample_data_mean, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence)) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$pop_sd / sqrt(input$sample_size))
      upper_bound <- mean(data$x) + ci * (input$pop_sd / sqrt(input$sample_size))
      contains_mean <- ifelse(input$pop_mean >= min(lower_bound, upper_bound) && input$pop_mean <= max(lower_bound, upper_bound), "TRUE", "FALSE")
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound, contains_mean = contains_mean)
    })
  })
  
  # Generate and store ci_data for proportion simulation
  observeEvent(input$num_intervals_prop, {
    sample_data_prop <- lapply(1:input$num_intervals_prop, function(i) {
      data.frame(y = i, x = rbinom(input$sample_size_prop, 1, input$pop_prop))
    })
    
    ci_data$prop <- lapply(sample_data_prop, function(data) {
      ci <- qnorm((1 - as.numeric(input$confidence_prop)) / 2, mean = 0, sd = 1)
      p_hat <- mean(data$x)
      se <- sqrt(p_hat * (1 - p_hat) / input$sample_size_prop)
      lower_bound <- p_hat - ci * se
      upper_bound <- p_hat + ci * se
      contains_prop <- ifelse(input$pop_prop >= min(lower_bound, upper_bound) && input$pop_prop <= max(lower_bound, upper_bound), TRUE, FALSE)
      data.frame(y = data$y, x = p_hat, xmin = lower_bound, xmax = upper_bound, contains_prop = contains_prop)
    })
  })
  
  # Render the plot for mean simulation
  output$conf_plot <- renderPlotly({
    gg <- ggplot() +
      geom_vline(xintercept = input$pop_mean, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data$mean), 
                     mapping = aes(y = y, xmin = xmax, xmax = xmin, color = contains_mean), 
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data$mean), 
                 mapping = aes(y = y, x = x, color = contains_mean)) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#882255"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Mean",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) %>%
      event_register("plotly_selected")
  })
  
  # Render the plot for proportion simulation
  output$conf_plot_prop <- renderPlotly({
    gg <- ggplot() +
      geom_vline(xintercept = input$pop_prop, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data$prop),
                     mapping = aes(y = y, xmin = xmax, xmax = xmin, color = contains_prop),
                     height = 0.2) +
      geom_point(data = do.call(rbind, ci_data$prop), 
                 mapping = aes(y = y, x = x, color = contains_prop)) +
      scale_color_manual(values = c("TRUE" = "#009E73", "FALSE" = "#882255"), guide = FALSE, labels = c("TRUE" = "Contains Parameter", "FALSE" = "Doesn't Contain Parameter")) +
      labs(title = "Confidence Intervals",
           x = "Proportion",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) %>%
      event_register("plotly_selected")
  })
  
  # Calculate number of intervals containing the parameter
  observe({
    if (!is.null(event_data("plotly_selected"))) {
      selected_data <- event_data("plotly_selected")
      print(selected_data)
      selected_intervals(do.call(rbind, selected_data))
    }
  })
  
  # Calculate number of intervals containing the parameter
  output$intervals_containing_param_prop <- renderText({
    if (is.null(selected_intervals())) {
      "Select intervals on the plot to calculate."
    } else {
      total_intervals <- nrow(selected_intervals())
      true_intervals <- sum(selected_intervals()$contains_prop)
      paste("Number of Intervals Containing Parameter:", true_intervals, "/", total_intervals)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)






