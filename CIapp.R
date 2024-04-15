# ################################################################################
# # This app demonstrates the concept of confidence intervals for various contexts,  
# # specifically the ideas such as confidence intervals are not fixed, they might not 
# # contain the "true" population parameter, how CIs are affected by sample sizes, 
# # the interpretation of CIs.
# ################################################################################
# # Required libraries
# library(tidyverse)
# library(mosaic)
# library(shiny)
# ################################################################################
# 
# ui <- fluidPage(
#   # Application title
#   titlePanel("Simulated Confidence Intervals"),
#   # Sidebar with a numeric input for month
#   fluidRow(
#     column(12,
#            selectInput(inputId = "context",
#                        label = "Statistic:",
#                        choices = c("One Proportion", "One Mean"),
#                        selected = "One Proportion"),
#            sliderInput(inputId = "true_value",
#                        label = "True parameter value:",
#                        min = 0, max = 1, value = 0.5),
#            sliderInput(inputId = "sample_size",
#                        label = "Sample size (n):",
#                        min = 1, max = 100, value = 50),
#            sliderInput(inputId = "numCI",
#                        label = "Number of intervals:",
#                        min = 1, max = 100, value = 50),
#            actionButton("generate_btn", "Generate Graphs")  # Button to trigger graph generation
#     )
#   ),
#   # Show a plot of the generated distribution
#   fluidRow(
#     column(4, 
#            plotOutput("hist")),
#     column(4,
#            plotOutput("boxplot")),
#     column(4, 
#            tableOutput("table"))
#   )
# )
# 
# 
# 
# 
# 
# 
# 
# server <- function(input, output, session) {
#   
#   # Define a reactive function to generate the data frame
#   df <- reactive({
#     
#     ss <- as.integer(input$sample_size)
#     tv <- as.numeric(input$true_value)
#     nCI <- as.integer(input$numCI)
#     
#     # Generate empty matrix to store confidence intervals
#     confidence_intervals <- matrix(NA, nrow = nCI, ncol = 2)
#     
#     # Simulate confidence intervals
#     for (i in 1:nCI) {
#       # Generate sample data
#       sample_data <- rbinom(ss, 1, tv)
#       # Calculate sample proportion
#       sample_proportion <- mean(sample_data)
#       # Calculate standard error
#       se <- sqrt(sample_proportion * (1 - sample_proportion) / ss)
#       # Calculate margin of error
#       margin_of_error <- qnorm(0.975) * se
#       # Calculate confidence interval
#       confidence_intervals[i, ] <- c(sample_proportion - margin_of_error, sample_proportion + margin_of_error)
#     }
#     
#     print(confidence_intervals)  # Debugging: Print confidence intervals
#     return(confidence_intervals)
#   })
#   
#   # Render the histogram plot
#   output$hist <- renderPlot({
#     req(df())  # Ensure df() is not NULL before rendering plot
#     
#     # Create a data frame for plotting
#     df_plot <- data.frame(Interval = 1:nrow(df()),
#                           Lower = df()[, 1],
#                           Upper = df()[, 2])
#     
#     # Plot the confidence intervals
#     ggplot(df_plot, aes(x = Interval)) +
#       
#       geom_segment(aes(x = Interval, xend = Interval, y = Lower, yend = Upper), color = "black") +
#       coord_flip() +
#       labs(x = "Interval Number", y = "Confidence Interval", title = "Simulated Confidence Intervals") +
#       scale_x_continuous(expand = c(0, 0))
#   })
#   
#   
#   
#   # Render the table
#   output$table <- renderTable({
#     req(df())  # Ensure df() is not NULL before rendering table
#     # Create a data frame for the table
#     data.frame(Interval = 1:nrow(df()), Lower = df()[, 1], Upper = df()[, 2])
#   })
#   
# }
# 
# 
# 
# 
# # Run the application
# shinyApp(ui = ui, server = server)
################################################################################

library(tidyverse)
library(plotly)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Simulating Confidence Intervals"), # Application title 
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", "Population mean (μ):", value = 0.5, step = 0.1),
      numericInput("sd", "Population standard deviation (σ):", value = 10),
      numericInput("n", "Sample size (n):", value = 50),
      numericInput("n_intervals", "Number of intervals:", value = 10),
      sliderInput("confidence", "Confidence Level:",
                  min = 0.01, max = 0.99, value = 0.95, step = 0.01),
      textOutput("intervals_containing_mu") # display number of intervals containing μ and percentage
    ),
    mainPanel(
      plotlyOutput("conf_plot") # plotly allows for plots with interactive features 
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculate number of intervals containing μ for mean simulation
  intervals_containing_mu_mean <- reactive({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data <- lapply(1:input$n_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$n, mean = input$mean, sd = input$sd)) # normal distribution!
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$sd / sqrt(input$n))
      upper_bound <- mean(data$x) + ci * (input$sd / sqrt(input$n))
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound)
    })
    
    # Calculate number of intervals containing μ
    sum(sapply(ci_data, function(ci) {
      if (input$mean >= min(ci$xmin, ci$xmax) && input$mean <= max(ci$xmin, ci$xmax)) {
        1
      } else {
        0
      }
    }))
  })
  
  # Display number of intervals containing μ and percentage for mean simulation
  output$intervals_containing_mu <- renderText({
    total_intervals <- input$n_intervals
    intervals_with_mu <- intervals_containing_mu_mean()
    percentage <- round(intervals_with_mu / total_intervals * 100, 2)
    paste("Number of intervals containing μ:", intervals_with_mu, "/", total_intervals, "=", percentage, "%")
  })
  
  # Render the plot for mean simulation
  output$conf_plot <- renderPlotly({
    # Generate data
    set.seed(123) # for reproducibility
    sample_data <- lapply(1:input$n_intervals, function(i) {
      data.frame(y = i, x = rnorm(input$n, mean = input$mean, sd = input$sd)) # normal distribution
    })
    
    # Calculate confidence intervals for each sample
    ci_data <- lapply(sample_data, function(data) {
      ci <- qnorm((1 - input$confidence) / 2, mean = 0, sd = 1)
      lower_bound <- mean(data$x) - ci * (input$sd / sqrt(input$n))
      upper_bound <- mean(data$x) + ci * (input$sd / sqrt(input$n))
      data.frame(y = data$y, x = mean(data$x), xmin = lower_bound, xmax = upper_bound)
    })
    
    # Create plot
    gg <- ggplot() +
      geom_vline(xintercept = input$mean, linetype = "dashed", color = "#D55E00") +
      geom_errorbarh(data = do.call(rbind, ci_data), 
                     mapping = aes(y = y, xmin = xmin, xmax = xmax), 
                     color = "#009E73", height = 0.2) +
      geom_point(data = do.call(rbind, ci_data), 
                 mapping = aes(y = y, x = x), color = "#009E73") +
      labs(title = "Confidence Intervals",
           x = "Mean",
           y = "Interval") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Hide y-axis text
            axis.title.y = element_blank()) # Hide y-axis label
    
    ggplotly(gg) # for converting ggplot2 figures into interactive ones powered by plotly (using js code)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
