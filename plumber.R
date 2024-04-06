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
library(shiny)
library(shiny)
library(shiny)





library(shiny)

theta <- seq(0, 2 * pi, length.out = 100)

ui <- fluidPage(
  titlePanel("Sampling Visualization"),
  fluidRow(
    column(
      width = 4,
      plotOutput("scatter_plot1")
    ),
    column(
      width = 4,
      plotOutput("scatter_plot2")
    ),
    column(
      width = 4,
      numericInput("sample_size", "Sample Size:", value = 5, min = 1, max = 20),
      selectInput("sample_type", "Sample Type:", choices = c("Random Sampling", "Stratified Sampling", "Cluster Sampling")),
      uiOutput("cluster_input"),
      actionButton("sample_btn", "Sample")
    )
  )
)

server <- function(input, output) {
  # Initialize num_clusters with a default value
  num_clusters <- reactiveVal(3)
  
  # Generate random coordinates within the circle
  plot_data <- reactive({
    num_points <- 50
    radius <- 1.7  # Adjust the radius
    theta <- runif(num_points, min = 0, max = 2 * pi)
    r <- sqrt(runif(num_points, min = 0, max = radius^2))
    x <- r * cos(theta)
    y <- r * sin(theta)
    numbers <- 1:num_points
    
    data.frame(x = x, y = y, numbers = numbers)
  })
  
  stratified_colors_df <- reactiveVal(NULL)
  cluster_colors_df <- reactiveVal(NULL)
  clusters <- reactiveVal(NULL)
  cluster_lines <- reactiveVal(NULL)
  
  output$scatter_plot1 <- renderPlot({
    plot_data_df <- plot_data()
    # Plot scatter plot with numbers
    plot(plot_data_df$x, plot_data_df$y, type = "n", xlab = "", ylab = "", xlim = c(-1.8, 1.8), ylim = c(-1.8, 1.8), main = "Population", axes = FALSE, asp = 1)
    
    if (input$sample_type == "Stratified Sampling") {
      if (is.null(stratified_colors_df())) {
        assigned_colors <- rep(c("red", "blue"), length.out = nrow(plot_data_df))
        stratified_colors_df(data.frame(numbers = plot_data_df$numbers, color = assigned_colors))
      }
      
      assigned_colors <- stratified_colors_df()
      
      for (i in seq(nrow(plot_data_df))) {
        color <- assigned_colors[i, "color"]
        text(plot_data_df$x[i], plot_data_df$y[i], labels = plot_data_df$numbers[i], cex = 1.2, col = color)
      }
    } else if (input$sample_type == "Cluster Sampling") {
      if (is.null(cluster_colors_df())) {
        assigned_colors <- rep(c("darkgreen", "purple", "orange", "blue"), length.out = nrow(plot_data_df))
        cluster_colors_df(data.frame(numbers = plot_data_df$numbers, color = assigned_colors))
      }
      
      assigned_colors <- cluster_colors_df()
      
      for (i in seq(nrow(plot_data_df))) {
        color <- assigned_colors[i, "color"]
        text(plot_data_df$x[i], plot_data_df$y[i], labels = plot_data_df$numbers[i], cex = 1.2, col = color)
      }
      
      # Perform k-means clustering using num_clusters
      clusters_data <- kmeans(plot_data_df[, c("x", "y")], centers = num_clusters())
      clusters(clusters_data$cluster)
      
      # Plot clusters with organic boundaries
      for (i in unique(clusters())) {
        cluster_points <- plot_data_df[clusters() == i, ]
        hull <- chull(cluster_points$x, cluster_points$y)
        hull <- c(hull, hull[1]) # Add the first point to close the polygon
        lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "red")
        cluster_lines_data <- cluster_lines()
        if (is.null(cluster_lines_data)) {
          cluster_lines_data <- list()
        }
        cluster_lines_data[[i]] <- cbind(cluster_points$x[hull], cluster_points$y[hull])
        cluster_lines(cluster_lines_data)
      }
    } else {
      text(plot_data_df$x, plot_data_df$y, labels = plot_data_df$numbers, cex = 1.2, col = "black")
    }
    
    # Draw circle around the cluster of numbers
    lines(1.9 * cos(theta), 1.9 * sin(theta), col = "black", lwd = 2)
    
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
    plot(sample_data$x, sample_data$y, type = "n", xlab = "", ylab = "", xlim = c(-1.8, 1.8), ylim = c(-1.8, 1.8), main = "Sampled Subset", axes = FALSE, asp = 1)
    
    if (input$sample_type == "Stratified Sampling") {
      assigned_colors <- stratified_colors_df()
      for (i in seq(nrow(sample_data))) {
        number <- sample_data$numbers[i]
        color <- assigned_colors[assigned_colors$numbers == number, "color"]
        text(sample_data$x[i], sample_data$y[i], labels = number, cex = 1.2, col = color)
      }
    } else if (input$sample_type == "Cluster Sampling") {
      assigned_colors <- cluster_colors_df()
      cluster_lines_data <- cluster_lines()
      for (i in seq_along(cluster_lines_data)) {
        lines(cluster_lines_data[[i]][, 1], cluster_lines_data[[i]][, 2], lty = 2, col = "red")
      }
      for (i in seq(nrow(sample_data))) {
        number <- sample_data$numbers[i]
        color <- assigned_colors[assigned_colors$numbers == number, "color"]
        text(sample_data$x[i], sample_data$y[i], labels = number, cex = 1.2, col = color)
      }
    } else {
      text(sample_data$x, sample_data$y, labels = sample_data$numbers, cex = 1.2, col = "black")
    }
    
    # Draw circle around the cluster of numbers
    lines(1.9 * cos(theta), 1.9 * sin(theta), col = "black", lwd = 2)
  })
  
  output$cluster_input <- renderUI({
    if (input$sample_type == "Cluster Sampling") {
      numericInput("num_clusters", "Number of Clusters:", value = 3, min = 1)
    }
  })
}

shinyApp(ui = ui, server = server)


