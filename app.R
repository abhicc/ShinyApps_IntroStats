library(sn)
library(tidyverse)
library(mosaic)
library(fGarch)

ui <- fluidPage(
  # Application title
  titlePanel("Summarizing Numerical Data"),
  # Sidebar with a numeric input for month
  fluidRow(
    
    column(3,
           numericInput(inputId = "mean_value",
                        label = "Input a mean value",
                        min = -25,
                        max = 25,
                        value = 0,
                        step = 0.1)),
    
    column(3,
           numericInput(inputId = "sd_value",
                        label = "Input a standard deviation",
                        min = 1,
                        max = 20,
                        value = 10, 
                        step = 1)),
    
    column(3,
           selectInput(inputId = "shape",
                       label = "Select a shape",
                       choices = c("Symmetric", "Positively Skewed", "Negatively Skewed"),
                       selected = "Symmetric"))
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
    
    if(input$shape == "Symmetric")
    {
      val <- rnorm(1000, mean = input$mean_value, sd = input$sd_value)
      df <- data.frame(value = val)
    }
    else if (input$shape == "Positively Skewed")
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
