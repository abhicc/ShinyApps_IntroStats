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
library(shiny)
library(ggplot2)
library(psych)

library(shiny)

ui <- fluidPage(
  titlePanel("Sampling Visualization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sample_size", "Sample Size:", value = 5, min = 1, max = 20),
      selectInput("sample_type", "Sample Type:", choices = c("Random Sampling", "Stratified Sampling")),
      actionButton("sample_btn", "Sample")
    ),
    mainPanel(
      plotOutput("scatter_plot1"),
      plotOutput("scatter_plot2")
    )
  )
)

server <- function(input, output) {
  # Generate random coordinates within the circle
  plot_data <- reactive({
    num_points <- 10
    radius <- 1
    theta <- runif(num_points, min = 0, max = 2 * pi)
    r <- sqrt(runif(num_points, min = 0, max = radius^2))
    x <- r * cos(theta)
    y <- r * sin(theta)
    numbers <- 1:num_points
    
    data.frame(x = x, y = y, numbers = numbers)
  })
  
  output$scatter_plot1 <- renderPlot({
    plot_data_df <- plot_data()
    # Plot scatter plot with numbers
    plot(plot_data_df$x, plot_data_df$y, type = "n", xlab = "", ylab = "", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), main = "Sampling Visualization")
    
    if (input$sample_type == "Stratified Sampling") {
      strata <- sample(1:5, nrow(plot_data_df), replace = TRUE)
      colors <- rainbow(5)
      text(plot_data_df$x, plot_data_df$y, labels = plot_data_df$numbers, cex = 1.2, col = colors[strata])
    } else {
      text(plot_data_df$x, plot_data_df$y, labels = plot_data_df$numbers, cex = 1.2, col = "black")
    }
    
    # Draw circle around the cluster of numbers
    theta <- seq(0, 2 * pi, length.out = 100)
    circle_x <- cos(theta)
    circle_y <- sin(theta)
    lines(circle_x, circle_y, col = "blue", lwd = 2)
    
    # Return coordinates for sampling
    plot_data_df
  })
  
  output$scatter_plot2 <- renderPlot({
    req(input$sample_btn)
    
    # Get coordinates and numbers from the first plot
    plot_data_df <- plot_data()
    
    # Sample random subset
    sample_numbers <- sample(plot_data_df$numbers, input$sample_size)
    sample_data <- plot_data_df[plot_data_df$numbers %in% sample_numbers, ]
    
    # Plot sampled subset
    plot(sample_data$x, sample_data$y, type = "n", xlab = "", ylab = "", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), main = "Sampled Subset")
    
    if (input$sample_type == "Stratified Sampling") {
      strata <- sample(1:5, nrow(sample_data), replace = TRUE)
      colors <- rainbow(5)
      text(sample_data$x, sample_data$y, labels = sample_data$numbers, cex = 1.2, col = colors[strata])
    } else {
      text(sample_data$x, sample_data$y, labels = sample_data$numbers, cex = 1.2, col = "black")
    }
    
    # Draw circle around the cluster of numbers
    cluster_center <- c(mean(sample_data$x), mean(sample_data$y))
    cluster_radius <- 1.5 * max(sqrt((sample_data$x - cluster_center[1])^2 + (sample_data$y - cluster_center[2])^2))
    theta <- seq(0, 2 * pi, length.out = 100)
    circle_x <- cluster_center[1] + cluster_radius * cos(theta)
    circle_y <- cluster_center[2] + cluster_radius * sin(theta)
    lines(circle_x, circle_y, col = "blue", lwd = 2)
  })
}

shinyApp(ui = ui, server = server)

