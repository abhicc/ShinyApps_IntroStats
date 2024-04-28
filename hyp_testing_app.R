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
      numericInput("sample_size", "Sample Size: n =", value = 100, step = 1),
      numericInput("success_count", "Count (Number of Successes):", value = 50, step = 1),
      numericInput("null_value", "Null Hypothesis: p =", value = 0.5, step = 0.1),
      selectInput("alternative", "Alternative Hypothesis:",
                  choices = c("not equal to", "less than", "greater than")),
      actionButton("generate_samples", "Generate 1000 Samples"),
      actionButton("reset_samples", "Reset Plot")
    ),
    
    mainPanel(
      h3(textOutput("total_samples_text")),  # Display total number of samples
      plotOutput("histogram"),
      h4(textOutput("p_hat_text")),  # Display p_hat value
      plotOutput("density_plot")  # Display standard normal density plot
    )
  )
)

# Initialize samples
samples <- NULL

# Server
server <- function(input, output, session) {
  
  observeEvent(input$generate_samples, {
    n <- input$sample_size
    p_hat <- input$success_count / n
    
    # Determine the alternative hypothesis
    alternative <- switch(input$alternative,
                          "not equal to" = "two.sided",
                          "less than" = "less",
                          "greater than" = "greater")
    
    # Generate 1000 samples
    new_samples <- replicate(1000, rbinom(1, size = n, prob = input$null_value))
    
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
      ggplot(df, aes(x = proportions, fill = ifelse(alternative == "two.sided" & proportions != p_hat, 
                                                    "Below/Above", 
                                                    ifelse(proportions < p_hat & alternative == "not equal to", 
                                                           "Below", 
                                                           ifelse(proportions > p_hat & alternative == "not equal to", 
                                                                  "Above", 
                                                                  ifelse(proportions <= p_hat & alternative == "less", 
                                                                         "Below", 
                                                                         ifelse(proportions >= p_hat & alternative == "greater", 
                                                                                "Above", 
                                                                                "Not Selected"))))))) +
        geom_histogram(binwidth = 0.05, color = "black") +
        scale_fill_manual(values = c("not equal to" = "#009E73", 
                                     "less than" = "#009E73", 
                                     "greater than" = "#009E73", 
                                     "Below" = "#009E73", 
                                     "Above" = "#009E73", 
                                     "Below/Above" = "gray"), name = "Alternative Hypothesis") +
        geom_vline(xintercept = p_hat, linetype = "dashed", color = "#D55E00", size = 1) +  # Add vertical line at p hat
        labs(title = "Randomization Histogram of Proportions",
             x = "Proportion (p)", y = "Frequency") +
        theme_minimal()
    })
    
    # Update total number of samples
    output$total_samples_text <- renderText({
      paste("Total Samples:", length(samples))
    })
    
    # Display p_hat value
    output$p_hat_text <- renderText({
      paste("p_hat =", round(p_hat, 3))
    })
    
    # Plot standard normal density plot
    output$density_plot <- renderPlot({
      ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = dnorm, color = "blue", fill = "lightblue", alpha = 0.5, args = list(mean = 0, sd = 1)) +
        labs(title = "Standard Normal Density Plot",
             x = "x", y = "Density") +
        theme_minimal()
    })
  })
  
  # Reset samples and histogram
  observeEvent(input$reset_samples, {
    samples <<- NULL
    output$histogram <- renderPlot(NULL)
    output$total_samples_text <- renderText("Total Samples: 0")
    output$p_hat_text <- renderText(NULL)
    output$density_plot <- renderPlot(NULL)
  })
}

shinyApp(ui = ui, server = server)
