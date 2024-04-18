# Required libraries
library(tidyverse)
library(mosaic)
library(shiny)

theta <- seq(0, 2 * pi, length.out = 100)

ui <- fluidPage(
  titlePanel("Sampling Methods"),
  fluidRow(
    column(
      width = 8,
      plotOutput("scatter_plot")
    ),
    column(
      width = 4,
      sliderInput("sample_size", "Sample Size:", value = 5, min = 1, max = 20),
      selectInput("sample_type", "Sample Type:", choices = c("Random Sampling", "Stratified Sampling", "Cluster Sampling")),
      uiOutput("cluster_input"),
      actionButton("sample_btn", "Sample")
    )
  )
)

server <- function(input, output, session) {
  # Initialize num_clusters with a default value
  num_clusters <- reactiveVal(3)
  
  # Generate random coordinates within the rectangle
  plot_data <- reactive({
    num_points <- 50
    x <- runif(num_points, min = -2.8, max = 2.8)
    y <- runif(num_points, min = -1.9, max = 1.9)  # Adjusted y-limits
    
    data.frame(x = x, y = y)
  })
  
  sample_data <- reactiveVal(NULL)
  
  observeEvent(input$sample_btn, {
    req(input$sample_btn)
    
    # Get coordinates from the first plot
    plot_data_df <- plot_data()
    
    if (input$sample_type == "Cluster Sampling") {
      # Perform k-means clustering
      clusters <- kmeans(plot_data_df, centers = input$num_clusters)
      
      # Sample all points from one cluster
      cluster_to_sample <- sample(1:max(clusters$cluster), 1)
      
      # Get all points from the selected cluster
      selected_cluster_df <- plot_data_df[clusters$cluster == cluster_to_sample, ]
      
      # Sample all points from the selected cluster
      sample_data_df <- selected_cluster_df
      
      # Remove points not in the selected cluster
      plot_data_df <- selected_cluster_df
    } else {
      # Sample random subset
      sample_numbers <- sample(1:nrow(plot_data_df), input$sample_size)
      sample_data_df <- plot_data_df[sample_numbers, ]
    }
    
    sample_data(sample_data_df)
  })
  
  
  
  
  output$scatter_plot <- renderPlot({
    plot_data_df <- plot_data()
    plot(plot_data_df$x, plot_data_df$y, type = "n", xlab = "", ylab = "", xlim = c(-2.8, 2.8), ylim = c(-1.9, 1.9), main = "Sampling Methods", axes = FALSE, asp = 1)
    
    # Draw rectangle around the plot area
    rect(-3, -1.9, 3, 1.9, border = "black", lwd = 2)
    
    # Plot all points
    points(plot_data_df$x, plot_data_df$y, pch = 16, col = "black")
    
    # Highlight sampled points with circles
    if (!is.null(sample_data())) {
      points(sample_data()$x, sample_data()$y, pch = 21, bg = "red", cex = 1.5)
    }
    
    # Add clusters for stratified sampling and cluster sampling
    if (input$sample_type == "Cluster Sampling") {
      # Perform k-means clustering
      clusters <- kmeans(plot_data_df, centers = 3)
      
      # Get the total number of points
      total_points <- nrow(plot_data_df)
      
      # Shuffle colors for all points
      shuffle_colors <- sample(1:total_points)
      
      # Plot clusters with mixed colors
      for (i in 1:max(clusters$cluster)) {
        cluster_points <- plot_data_df[clusters$cluster == i, ]
        # Assign shuffled colors to points within the cluster
        points(cluster_points$x, cluster_points$y, col = shuffle_colors[clusters$cluster == i], pch = 16)
      }
      
      # Add dashed lines connecting points within each cluster
      for (i in 1:max(clusters$cluster)) {
        cluster_points <- plot_data_df[clusters$cluster == i, ]
        hull <- chull(cluster_points$x, cluster_points$y)
        hull <- c(hull, hull[1]) # Add the first point to close the polygon
        lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "black")
      }
    }
    
    if (input$sample_type == "Stratified Sampling") {
      # Perform k-means clustering
      clusters <- kmeans(plot_data_df, centers = 3)
      
      # Plot clusters with mixed colors
      for (i in 1:max(clusters$cluster)) {
        cluster_points <- plot_data_df[clusters$cluster == i, ]
        # Shuffle colors within each cluster
        shuffle_colors <- sample(1:nrow(cluster_points))
        points(cluster_points$x, cluster_points$y, col = shuffle(colors(), nrow(cluster_points)), pch = 16)
      }
      
      # Duplicate clusters for coloring
      duplicate_clusters <- clusters$cluster + max(clusters$cluster)
      
      # Plot duplicate clusters
      points(plot_data_df$x, plot_data_df$y, col = duplicate_clusters, pch = 16)
      
      # Add dashed lines connecting points within each cluster
      for (i in 1:max(clusters$cluster)) {
        cluster_points <- plot_data_df[clusters$cluster == i, ]
        hull <- chull(cluster_points$x, cluster_points$y)
        hull <- c(hull, hull[1]) # Add the first point to close the polygon
        lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "black")
      }
    }
    
  })
  output$cluster_input <- renderUI({
    if (input$sample_type == "Cluster Sampling") {
      sliderInput("num_clusters", "Number of Clusters:", min = 1, max = 2, value = 1, step = 1)
    }
  })
}

shinyApp(ui = ui, server = server)
