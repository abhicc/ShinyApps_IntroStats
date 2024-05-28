##########################################################################################################
# Randomization tests for single proportions are nonparametric statistical tests used to assess the 
# significance of differences or associations involving a single proportion or proportion-like data.

# Samples of the same size as the original sample are drawn from a theoretical distribution with a proportion 
# equal to the hypothesized population proportion (i.e., the value in the null hypothesis).
# The sample proportion in each randomization sample is recorded on the randomization histogram.  
##########################################################################################################
library(shiny)
library(ggplot2)
library(mosaic)

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
      actionButton("reset_samples", "Reset Plot"),
      checkboxInput("show_density_plot", "Show Density Plot", value = FALSE)  # Checkbox to show/hide density plot
    ),
    
    mainPanel(
      h3(textOutput("total_samples_text")),  # Display total number of samples
      plotOutput("histogram"),
      h4(textOutput("p_hat_text")),  # Display p_hat value
      h4(textOutput("samples_out_of_1000_text")),  # Display count of samples out of 1000
      h4(textOutput("p_value_text")),
      conditionalPanel(
        condition = "input.alternative == 'less than'",
        plotOutput("density_plot_less_than")  # Display density plot for less than
      ),
      conditionalPanel(
        condition = "input.alternative == 'greater than'",
        plotOutput("density_plot_greater_than")  # Display density plot for greater than
      ),
      conditionalPanel(
        condition = "input.alternative == 'not equal to'",
        plotOutput("density_plot_not_equal_to")  # Display density plot for not equal to
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      p("App created by Nina Austria and maintained by Abhishek Chakraborty", align = "center", style = "margin-top: 20px;")
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
    
    # 1000 repetitions
    new_samples <- mosaic::do(1000) * mosaic::rflip(n, prob = input$null_value)
    
    # Add new samples to previous samples
    if (is.null(samples)) {
      samples <<- new_samples
    } else {
      samples <<- rbind(samples, new_samples)
    }
    
    # Count of samples based on alternative hypothesis
    if (alternative == "less") {
      samples_out_of_1000 <- sum(samples$prop < p_hat)
      p_value <- prop(~ prop < p_hat, data = samples)      # Calculate p-value
    } else if (alternative == "greater") {
      samples_out_of_1000 <- sum(samples$prop > p_hat)
      p_value <- prop(~ prop > p_hat, data = samples)     # Calculate p-value
    } else {
      if (p_hat < input$null_value) {
        samples_out_of_1000 <- sum(samples$prop < p_hat) + sum(samples$prop > (1-p_hat))
        p_value <- prop(~ prop < p_hat, data = samples) * 2      # Calculate p-value
      } else {
        samples_out_of_1000 <- sum(samples$prop > p_hat) + sum(samples$prop < (1-p_hat))
        p_value <- prop(~ prop > p_hat, data = samples) * 2     # Calculate p-value
      }
    }
    
    # Convert proportions to data frame for ggplot
    df <- data.frame(prop = samples$prop)
    
    # Plot histogram using ggplot
    output$histogram <- renderPlot({
        ggplot(df, aes(x = prop, fill = ifelse(alternative == "two.sided" & prop != p_hat, 
                                                      "Below/Above", 
                                                      ifelse(prop < p_hat & alternative == "not equal to", 
                                                             "Below", 
                                                             ifelse(prop > p_hat & alternative == "not equal to", 
                                                                    "Above", 
                                                                    ifelse(prop <= p_hat & alternative == "less", 
                                                                           "Below", 
                                                                           ifelse(prop >= p_hat & alternative == "greater", 
                                                                                  "Above", 
                                                                                  "Not Selected"))))))) +
        geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) +
        scale_fill_manual(values = c("not equal to" = "#009E73", 
                                     "less than" = "#009E73", 
                                     "greater than" = "#009E73", 
                                     "Below" = "#009E73", 
                                     "Above" = "#009E73", 
                                     "Below/Above" = "gray"), name = "Alternative Hypothesis") +
        geom_vline(aes(xintercept = p_hat), color = "#D55E00", linetype = "dashed", size = 1) +
        labs(title = "Randomization Distribution of Proportion",
             x = "Proportion",
             y = "Frequency") +
        theme_minimal()
    })
    
    # Update total number of samples
    output$total_samples_text <- renderText({
      paste("Total number of samples:", nrow(samples))
    })
    
    # Display p_hat value
    output$p_hat_text <- renderText({
      paste("Sample proportion (p̂):", round(p_hat, 4))
    })
    
    # Display count of samples out of 1000
    output$samples_out_of_1000_text <- renderText({
      paste("Samples out of ", nrow(samples), ":", samples_out_of_1000)
    })
    
    # Display p value
    output$p_value_text <- renderText({
      paste("p-value:", round(p_value, 4))
    })
    
    # Plot standard normal density plot for less than
    output$density_plot_less_than <- renderPlot({
      if (input$show_density_plot) {
        # Calculate the test statistic
        test_statistic <- (p_hat - input$null_value) / sqrt(input$null_value * (1 - input$null_value) / n)
        
        # Generate x values for density plot
        x_values <- seq(-4, test_statistic, by = 0.01)
        
        # Calculate density values
        density_values <- dnorm(x_values)
        
        # Combine x and density values into a data frame
        df_density <- data.frame(x = x_values, density = density_values)
        
        # Plot the density plot for less than
        ggplot(df_density, aes(x = x)) +
          geom_line(aes(y = density), color = "black") +
          geom_area(aes(y = density), fill = "#009E73", alpha = 0.5) +
          geom_vline(xintercept = test_statistic, linetype = "dashed", color = "#D55E00") +  # Add vertical line at test statistic
          labs(title = "Standard Normal Density Plot (Less Than)",
               x = "x", y = "Density") +
          theme_minimal()
      } else {
        NULL  # Return NULL if density plot is not to be shown
      }
    })
    
    # Plot standard normal density plot for greater than
    output$density_plot_greater_than <- renderPlot({
      if (input$show_density_plot) {
        # Calculate the test statistic
        test_statistic <- (p_hat - input$null_value) / sqrt(input$null_value * (1 - input$null_value) / n)
        
        # Generate x values for density plot
        x_values <- seq(test_statistic, 4, by = 0.01)
        
        # Calculate density values
        density_values <- dnorm(x_values)
        
        # Combine x and density values into a data frame
        df_density <- data.frame(x = x_values, density = density_values)
        
        # Plot the density plot for greater than
        ggplot(df_density, aes(x = x)) +
          geom_line(aes(y = density), color = "black") +
          geom_area(aes(y = density), fill = "#009E73", alpha = 0.5) +
          geom_vline(xintercept = test_statistic, linetype = "dashed", color = "#D55E00") +  # Add vertical line at test statistic
          labs(title = "Standard Normal Density Plot (Greater Than)",
               x = "x", y = "Density") +
          theme_minimal()
      } else {
        NULL  # Return NULL if density plot is not to be shown
      }
    })
    
    # Plot standard normal density plot for not equal to
    output$density_plot_not_equal_to <- renderPlot({
      if (input$show_density_plot) {
        # Calculate the test statistic
        test_statistic <- (p_hat - input$null_value) / sqrt(input$null_value * (1 - input$null_value) / n)
        
        # Generate x values for density plot
        x_values <- seq(-4, 4, by = 0.01)
        
        # Calculate density values
        density_values <- dnorm(x_values)
        
        # Combine x and density values into a data frame
        df_density <- data.frame(x = x_values, density = density_values)
        
        # Plot the density plot for not equal to
        ggplot(df_density, aes(x = x)) +
          geom_line(aes(y = density), color = "black") +
          geom_area(aes(y = density), fill = "#009E73", alpha = 0.5) +
          geom_vline(xintercept = test_statistic, linetype = "dashed", color = "#D55E00") +  # Add vertical line at test statistic
          labs(title = "Standard Normal Density Plot (Not Equal To)",
               x = "x", y = "Density") +
          theme_minimal()
      } else {
        NULL  # Return NULL if density plot is not to be shown
      }
    })
  })
  
  # Reset samples, histogram, and density plots
  observeEvent(input$reset_samples, {
    samples <<- NULL
    output$histogram <- renderPlot(NULL)
    output$total_samples_text <- renderText("Total Samples: 0")
    output$p_hat_text <- renderText(NULL)
    output$samples_out_of_1000_text <- renderText(NULL)
    output$p_value_text <- renderText(NULL)
    output$density_plot_less_than <- renderPlot(NULL)
    output$density_plot_greater_than <- renderPlot(NULL)
    output$density_plot_not_equal_to <- renderPlot(NULL)
  })
}

shinyApp(ui = ui, server = server)
