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
           textInput(inputId = "true_value",
                     label = "Write a value for the 'true' parameter:",
                     value = ""),
           textInput(inputId = "sample_size",
                     label = "Sample size (n):",
                     value = ""),
           textInput(inputId = "numCI",
                     label = "Number of intervals:",
                     value = "")
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


server <- function(input, output) {
  
  df <- reactive({
    
    if(input$context == "One Proportion")
    {
      ss <- input$sample_size
      tv <- input$true_value
      nCI <- input$numCI
      
      CIdf <- matrix(NA, nrow = nCI, ncol = 2)
      
      for(i in 1:nCI)
      {
        responses <- rbernoulli(n = ss, p = tv)
        
        df <- matrix(NA, nrow = 1000, ncol = ss + 1)
        
        for(i in 1:nrow(df))
        {
          df[i,1:ss] = resample(responses, replace = TRUE)
          df[i, ss+1] = sum(df[i, 1:ss])/ss
        }
        
        CIdf[i, 1] = as.numeric(quantile(df[,ss+1], probs = c(0.025, 0.975))[1])
        CIdf[i, 2] = as.numeric(quantile(df[,ss+1], probs = c(0.025, 0.975))[2])
      }
      
    }
    else if (input$context == "One Mean")
    {
      val_right <- rsnorm(1000, mean = input$mean_value, sd = input$sd_value, xi = 3)
      df <- data.frame(value = val_right)
    }
    else 
    {
      val_left <- rsnorm(1000, mean = input$mean_value, sd = input$sd_value, xi = -3)  
      df <- data.frame(value = val_left)
    }
    
    # val_right <- c(val, tail(val[order(val)], 100) + rnorm(100, mean=input$mean_value+10, sd=2))
    # val_transform <- log10(val); df <- data.frame(val_transform)
    return(df)
  })
  
  
  output$hist <- renderPlot({
    
    ggplot(data = df()) +
      geom_histogram(mapping = aes(x = value), color = "black", fill = "lightgreen", bins = 100) +
      labs(x = "Value", y = "Frequency", title = "Histogram") +
      # geom_vline(mapping = aes(xintercept = mean(df()$value)), color = "red")+
      # geom_vline(mapping = aes(xintercept = median(df()$value)), color = "blue", linetype = 3) +
      xlim(c(-100, 100))
    
  })
  
  output$boxplot <- renderPlot({
    
    ggplot(data = df()) +
      geom_boxplot(mapping = aes(x = value), color = "black", fill = "lightgreen") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = "Value", y = "", title = "Boxplot") +
      xlim(c(-100, 100))
    
  })
  
  
  
  output$table <- renderTable({
    
    res <- favstats(~ value, data=df())
    ftable <- data.frame(Summary = c("min", "Q1", "median", "Q3", "max", "IQR", "range"),
                         Values = c(round(res$min,3), round(res$Q1,3), round(res$median,3), 
                                    round(res$Q3,3), round(res$max,3), 
                                    round(res$Q3,3) - round(res$Q1,3),
                                    round(res$max,3) - round(res$min,3)))
    
    ftable
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
################################################################################

