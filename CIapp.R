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

ui <- fluidPage(
  # Application title
  titlePanel("Simulated Confidence Intervals"),
  # Sidebar with a numeric input for month
  fluidRow(
    column(12,
           selectInput(inputId = "context",
                       label = "Statistic:",
                       choices = c("One Proportion", "One Mean"),
                       selected = "One Proportion"),
           sliderInput(inputId = "true_value",
                       label = "True parameter value:",
                       min = ifelse(input$context == "One Proportion", 0, -10),
                       max = ifelse(input$context == "One Proportion", 1, 10),
                       value = 0.5),
           
           sliderInput(inputId = "sample_size",
                       label = "Sample size (n):",
                       min = 1, max = 100, value = 50),
           sliderInput(inputId = "numCI",
                       label = "Number of intervals:",
                       min = 1, max = 100, value = 50),
           actionButton("generate_btn", "Generate Graphs")  # Button to trigger graph generation
    )
  ),
  # Show a plot of the generated distribution
  fluidRow(
    column(4, 
           plotOutput("hist")),
    column(4,
           plotOutput("boxplot")),
    column(4, 
           tableOutput("table"))
  )
)







server <- function(input, output, session) {
  
  # Define a reactive function to generate the data frame
  df <- reactive({
    
    ss <- as.integer(input$sample_size)
    tv <- as.numeric(input$true_value)
    nCI <- as.integer(input$numCI)
    
    # Generate empty matrix to store confidence intervals
    confidence_intervals <- matrix(NA, nrow = nCI, ncol = 2)
    
    # Simulate confidence intervals
    for (i in 1:nCI) {
      # Generate sample data
      sample_data <- rbinom(ss, 1, tv)
      # Calculate sample proportion
      sample_proportion <- mean(sample_data)
      # Calculate standard error
      se <- sqrt(sample_proportion * (1 - sample_proportion) / ss)
      # Calculate margin of error
      margin_of_error <- qnorm(0.975) * se
      # Calculate confidence interval
      confidence_intervals[i, ] <- c(sample_proportion - margin_of_error, sample_proportion + margin_of_error)
    }
    
    print(confidence_intervals)  # Debugging: Print confidence intervals
    return(confidence_intervals)
  })
  
  # Render the histogram plot
  output$hist <- renderPlot({
    req(df())  # Ensure df() is not NULL before rendering plot
    
    # Create a data frame for plotting
    df_plot <- data.frame(Interval = 1:nrow(df()),
                          Lower = df()[, 1],
                          Upper = df()[, 2])
    
    # Plot the confidence intervals
    ggplot(df_plot, aes(x = Interval)) +

            geom_segment(aes(x = Interval, xend = Interval, y = Lower, yend = Upper), color = "black") +
      coord_flip() +
      labs(x = "Interval Number", y = "Confidence Interval", title = "Simulated Confidence Intervals") +
      scale_x_continuous(expand = c(0, 0))
  })
  
  
  
  # Render the table
  output$table <- renderTable({
    req(df())  # Ensure df() is not NULL before rendering table
    # Create a data frame for the table
    data.frame(Interval = 1:nrow(df()), Lower = df()[, 1], Upper = df()[, 2])
  })
  
}




# Run the application
shinyApp(ui = ui, server = server)
################################################################################

