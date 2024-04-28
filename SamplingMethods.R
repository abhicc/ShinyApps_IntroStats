library(tidyverse)
library(shiny)

# Generate random coordinates within the rectangle
plot_data <- reactive({
  num_points <- 100
  x <- runif(num_points, min = -4.8, max = 4.8)
  y <- runif(num_points, min = -1.8, max = 1.8)  # Adjusted y-limits
  data.frame(x = x, y = y)
})

cluster_colors <- c("blue", "green", "red", "orange", "purple", "yellow", "cyan", "magenta", "brown", "grey")

# Initialize clusters within a reactive expression
clusters <- reactiveVal(NULL)

observe({
  # Call kmeans inside the observer to ensure it's executed reactively
  clusters_data <- kmeans(plot_data(), centers = 3)
  # Update the reactive value with the result
  clusters(clusters_data)
})

ui <- fluidPage(
  titlePanel("Sampling Methods"),
  fluidRow(
    column(
      width = 4,
      selectInput("sample_type", "Sample Type:", choices = c("Random Sampling", "Stratified Sampling", "Cluster Sampling"))
    ), 
    column(width = 5,
           uiOutput("cluster_input")), 
    column(width = 3,
           actionButton("sample_btn", "Sample", style = "margin-top: 25px;"))
  ), 
  fluidRow(
    column(
      width = 12,
      plotOutput("scatter_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store sampled clusters
  sampled_clusters <- reactiveValues(clusters = NULL)
  
  sample_data <- reactiveVal(NULL)
  
  observeEvent(input$sample_btn, {
    req(input$sample_btn)
    
    # Get coordinates from the first plot
    plot_data_df <- plot_data()
    
    if (input$sample_type == "Cluster Sampling") {
      # Perform k-means clustering
      clusters_data <- clusters()
      
      # Sample the specified number of clusters
      sampled_clusters$clusters <- sample(unique(clusters_data$cluster), as.numeric(input$num_clusters))
      
      # Initialize a dataframe to store sampled points
      sample_data_df <- data.frame(x = numeric(0), y = numeric(0), cluster = integer(0))
      
      # Calculate the total number of points to sample
      total_points_to_sample <- input$sample_size
      
      # Loop through all clusters and add their points to the sample dataframe
      for (i in unique(clusters_data$cluster)) {
        cluster_points <- plot_data_df[clusters_data$cluster == i, ]
        # Sample points from the current cluster with replacement until we reach the desired number of sampled points
        cluster_sampled <- cluster_points %>%
          sample_n(total_points_to_sample, replace = TRUE)
        sample_data_df <- bind_rows(sample_data_df, cluster_sampled)
      }
      
      # Add a flag to indicate sampled points
      sample_data_df <- sample_data_df %>%
        mutate(sampled = TRUE)
      
      # Update the reactive value with sampled data
      sample_data(sample_data_df)
    }  else if (input$sample_type == "Stratified Sampling") {
      clusters_data <- clusters()
      
      plot_data_df$cluster <- clusters_data$cluster
      
      # Print number of points in each cluster before filtering
      print(table(clusters_data$cluster))
      
      # Calculate the number of base samples per cluster
      sample_size_per_cluster <- floor(input$sample_size / 3)
      
      # Calculate remainder
      remainder <- input$sample_size %% 3
      
      # Sample base points from each data frame
      sampled_cluster1 <- plot_data_df %>% 
        filter(cluster == 1) %>%
        sample_n(sample_size_per_cluster + ifelse(remainder >= 1, 1, 0), replace = FALSE)
      sampled_cluster2 <- plot_data_df %>% 
        filter(cluster == 2) %>%
        sample_n(sample_size_per_cluster + ifelse(remainder >= 2, 1, 0), replace = FALSE)
      sampled_cluster3 <- plot_data_df %>% 
        filter(cluster == 3) %>%
        sample_n(sample_size_per_cluster, replace = FALSE)
      
      sample_data_df <- bind_rows(sampled_cluster1, sampled_cluster2, sampled_cluster3)
      
      # Add a flag to indicate sampled points for each cluster
      sample_data_df <- sample_data_df %>%
        mutate(sampled = TRUE)
      
      # Update the reactive value with sampled data
      sample_data(sample_data_df)
    } else {
      sample_data_df <- plot_data_df %>%
        sample_n(input$sample_size, replace = FALSE)
      
      # Update the reactive value with sampled data
      sample_data(sample_data_df)
    }
  })
  
  output$scatter_plot <- renderPlot({
    plot_data_df <- plot_data()
    plot(plot_data_df$x, plot_data_df$y, type = "n", xlab = "", ylab = "", xlim = c(-2.8, 2.8), ylim = c(-1.9, 1.9), main = input$sample_type, axes = FALSE, asp = 1)
    
    # Draw rectangle around the plot area
    rect(-5, -2, 5, 2, border = "black", lwd = 2)
    if (input$sample_type == "Cluster Sampling") {
      clusters_data <- clusters()
      
      if (!is.null(clusters_data)) { 
        # Determine the number of clusters
        num_clusters <- length(unique(clusters_data$cluster))
        
        # Define the number of colors per cluster
        colors_per_cluster <- 5
        
        # Calculate total colors needed
        total_colors <- num_clusters * colors_per_cluster
        
        # Repeat the cluster colors to match the required number of colors
        cluster_colors_cycle <- rep(cluster_colors, ceiling(total_colors / length(cluster_colors)))
        
        # Plot points with colors based on their cluster membership
        for (i in unique(clusters_data$cluster)) {
          cluster_points <- plot_data_df[clusters_data$cluster == i, ]
          num_points_in_cluster <- nrow(cluster_points)
          colors <- rep(cluster_colors_cycle[((i - 1) * colors_per_cluster + 1):min(total_colors, i * colors_per_cluster)], each = ceiling(num_points_in_cluster / colors_per_cluster))[1:num_points_in_cluster]
          points(cluster_points$x, cluster_points$y, col = colors, pch = 16)
        }
        
        # Add dashed lines connecting points within each cluster
        for (i in unique(clusters_data$cluster)) {
          cluster_points <- plot_data_df[clusters_data$cluster == i, ]
          hull <- chull(cluster_points$x, cluster_points$y)
          hull <- c(hull, hull[1]) # Add the first point to close the polygon
          lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "black")
        }
      }
    
      
        
      
    }  else if (input$sample_type == "Stratified Sampling") {
      clusters_data <- clusters()
      
      if (!is.null(clusters_data)) { # Add a check for NULL or empty clusters_data
        # Plot clusters with predefined colors
        for (i in 1:max(clusters_data$cluster)) {
          cluster_points <- plot_data_df[plot_data_df$cluster == i, ]
          points(cluster_points$x, cluster_points$y, col = cluster_colors[i], pch = 16)
        }
        
        # Duplicate clusters for coloring
        duplicate_clusters <- clusters_data$cluster + max(clusters_data$cluster)
        
        # Plot duplicate clusters
        points(plot_data_df$x, plot_data_df$y, col = duplicate_clusters, pch = 16)
        
        # Add dashed lines connecting points within each cluster
        for (i in 1:max(clusters_data$cluster)) {
          cluster_points <- plot_data_df[clusters_data$cluster == i, ]
          hull <- chull(cluster_points$x, cluster_points$y)
          hull <- c(hull, hull[1]) # Add the first point to close the polygon
          lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "black")
        }
      }
    } else { 
      # Plot all points in black
      points(plot_data_df$x, plot_data_df$y, pch = 16, col = "black")
    }
    
    # Add highlighting if sampling has been performed
    if (!is.null(sample_data())) {
      if (input$sample_type == "Cluster Sampling") {
        if (!is.null(sampled_clusters$clusters)) {
          for (i in sampled_clusters$clusters) {
            cluster_points <- plot_data_df[clusters_data$cluster == i, ]
            points(cluster_points$x, cluster_points$y, pch = 21, col = "red", cex = 2.5, lwd = 5)
          }
        }
      } else {
        points(sample_data()$x, sample_data()$y, pch = 21, col = "red", cex = 2.5, lwd = 5)
      }
    }
    
  })
  
  output$cluster_input <- renderUI({
    if (input$sample_type == "Cluster Sampling") {
      selectInput("num_clusters", "Number of Sampled Clusters:",
                  choices = c("1" = 1, "2" = 2))    
    } else {
      sliderInput("sample_size", "Sample Size:", value = 5, min = 1, max = 15)
    }
  })
  
  # Clear sample_data reactive value when sample type changes or sample button is clicked
  observeEvent(input$sample_type, {
    req(input$sample_btn)
    if (!is.null(sample_data()) && !is.null(input$sample_type)) {
      sample_data(NULL)
    }
  })
  
} 

shinyApp(ui = ui, server = server)
