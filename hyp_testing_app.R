##########################################################################################################
# Randomization tests for single proportions are nonparametric statistical tests used to assess the 
# significance of differences or associations involving a single proportion or proportion-like data.

# Samples of the same size as the original sample are drawn from a theoretical distribution with a proportion 
# equal to the hypothesized population proportion (i.e., the value in the null hypothesis).
# The sample proportion in each randomization sample is recorded on the randomization histogram.  
##########################################################################################################

library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Randomization Hypothesis Test for a Single Proportion"),
  
  sidebarLayout(
    sidebarPanel(
      # All inputs on sidebar
      numericInput("sample_size", "Sample Size:", value = 100, step = 1),
      numericInput("success_count", "Count (Number of Successes):", value = 50, step = 1),
      numericInput("null_value", "Null Hypothesis: p =", value = 0.5, step = 0.1),
      selectInput("alternative", "Alternative Hypothesis:",
                  choices = c("not equal to", "less than", "greater than")),
      actionButton("generate_samples", "Generate 1000 Samples")
    ),
    
    mainPanel(
      h3(textOutput("total_samples_text")),  # Display total number of samples
      plotOutput("histogram")
    )
  )
)

# Initialize samples
samples <- NULL

# Server
server <- function(input, output) {
  
  observeEvent(input$generate_samples, {
    n <- input$sample_size
    p <- input$success_count / n
    
    alternative <- switch(input$alternative,
                          "not equal to" = "two.sided",
                          "less than" = "less",
                          "greater than" = "greater")
    
    # Generate 1000 samples
    new_samples <- replicate(1000, rbinom(1, size = n, prob = p))
    
    # Add new samples to previous samples
    if (is.null(samples)) {
      samples <<- new_samples
    } else {
      samples <<- c(samples, new_samples)
    }
    
    # Calculate proportions for each sample
    proportions <- samples / n
    
    # Convert proportions to data frame for ggplot
    df <- data.frame(proportions)
    
    # Plot histogram using ggplot
    output$histogram <- renderPlot({
      ggplot(df, aes(x = proportions)) +
        geom_histogram(binwidth = 0.05, fill = "#009E73", color = "black") +
        geom_vline(xintercept = input$null_value, linetype = "dashed", color = "#D55E00", size = 1) +  # Add vertical line at p hat
        labs(title = "Randomization Histogram of Proportions",
             x = "Proportion (p)", y = "Frequency") +
        theme_minimal()
    })
    
    # Update total number of samples
    output$total_samples_text <- renderText({
      paste("Total Samples:", length(samples))
    })
    
    # Perform hypothesis test
    # You can use functions like t.test(), prop.test(), or other appropriate tests
    # For proportion tests, prop.test() might be useful
    # Example:
    # test_result <- prop.test(input$success_count, input$sample_size, p = input$null_value, alternative = alternative)
    
    # Output test result
    # print(test_result)
  })
}

shinyApp(ui = ui, server = server)
