library(tidyverse)
library(mosaic)
library(shiny)

# Generate random coordinates within the rectangle
plot_data <- reactive({
  num_points <- 50
  x <- runif(num_points, min = -4.4, max = 4.4)
  y <- runif(num_points, min = -1.8, max = 1.8)  # Adjusted y-limits
  data.frame(x = x, y = y)
})

# Initialize clusters within a reactive expression
clusters <- reactiveVal(NULL)

observe({
  # Call kmeans inside the observer to ensure it's executed reactively
  clusters_data <- kmeans(plot_data(), centers = 3)
  # Update the reactive value with the result
  clusters(clusters_data)
})

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        #sample_btn {
          background-color: #d3d3d3; /* Green */
          border: none;
          color: black;
          padding: 15px 32px;
          text-align: center;
          text-decoration: none;
          display: inline-block;
          font-size: 16px;
          margin: 4px 2px;
          transition-duration: 0.4s;
          cursor: pointer;
          border-radius: 8px;
        }

        #sample_btn:hover {
          background-color: #d3d3d3; /* Light Grey */
          color: white;
        }
        "
      )
    )
  ),
  titlePanel("Sampling Methods"),
  fluidRow(
    column(
      width = 4,
      selectInput("sample_type", "Sample Type:", choices = c("Random Sampling", "Stratified Sampling", "Cluster Sampling"))
    ), 
    column(width = 4,
           uiOutput("cluster_input")), 
    column(width = 1), # Blank column
    column(width = 3,
           actionButton("sample_btn", "Sample", style = "margin-top: 25px;"))
  ), 
  fluidRow(
    column(
      width = 12,
      textOutput("plot_title"),
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
    
    # Initialize remainder
    remainder <- 0
    
    if (input$sample_type == "Cluster Sampling") {
      # Perform k-means clustering
      clusters_data <- clusters()
      
      # Sample the specified number of clusters
      sampled_clusters$clusters <- sample(unique(clusters_data$cluster), as.numeric(input$num_clusters))
      
      # Initialize a dataframe to store sampled points
      sample_data_df <- data.frame(x = numeric(0), y = numeric(0), cluster = integer(0))
      
      # Loop through sampled clusters and add their points to the sample dataframe
      for (i in sampled_clusters$clusters) {
        cluster_points <- plot_data_df[clusters_data$cluster == i, ]
        # Sample points within the cluster based on the number of points in that cluster
        cluster_size <- nrow(cluster_points)
        cluster_sampled <- cluster_points %>%
          sample_n(cluster_size, replace = TRUE)
        # Store the cluster number for each sampled point
        cluster_sampled$cluster <- i
        sample_data_df <- bind_rows(sample_data_df, cluster_sampled)
      }
      
      # Add a flag to indicate sampled points
      sample_data_df <- sample_data_df %>%
        mutate(sampled = TRUE)
      
      # Update the reactive value with sampled data
      sample_data(sample_data_df)
    } else if (input$sample_type == "Stratified Sampling") {
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
    rect(-4.5, -1.9, 4.5, 1.9, border = "black", lwd = 2)
    
    if (input$sample_type == "Cluster Sampling") {
      clusters_data <- clusters()
      
      if (!is.null(clusters_data)) { # Add a check for NULL or empty clusters_data
        # Plot clusters with mixed colors
        for (i in 1:max(clusters_data$cluster)) {
          cluster_points <- plot_data_df[clusters_data$cluster == i, ]
          # Shuffle colors for points within each cluster
          shuffle_colors <- sample(1:nrow(cluster_points))
          points(cluster_points$x, cluster_points$y, col = shuffle(colors(), nrow(cluster_points)), pch = 16)
        }
        
        # Add dashed lines connecting points within each cluster
        for (i in 1:max(clusters_data$cluster)) {
          cluster_points <- plot_data_df[clusters_data$cluster == i, ]
          hull <- chull(cluster_points$x, cluster_points$y)
          hull <- c(hull, hull[1]) # Add the first point to close the polygon
          lines(cluster_points$x[hull], cluster_points$y[hull], lty = 2, col = "black")
        }
        
      }
    } else if (input$sample_type == "Stratified Sampling") {
      clusters_data <- clusters()
      
      if (!is.null(clusters_data)) { # Add a check for NULL or empty clusters_data
        # Plot clusters with mixed colors
        for (i in 1:max(clusters_data$cluster)) {
          cluster_points <- plot_data_df[plot_data_df$cluster == i, ]
          # Shuffle colors within each cluster
          shuffle_colors <- sample(1:nrow(cluster_points))
          points(cluster_points$x, cluster_points$y, col = shuffle(colors(), nrow(cluster_points)), pch = 16)
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
      sliderInput("sample_size", "Sample Size:", value = 5, min = 1, max = 20)
    }
  })
  

} 

shinyApp(ui = ui, server = server)

