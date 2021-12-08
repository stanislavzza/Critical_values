#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Critical value calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("zscore",
                  "t-score:",
                  min = -5,
                  max = 5,
                  step = .1,
                  value = 0),
      sliderInput("sample_size",
                  "Sample size (df + 1)",
                  min = 2,
                  max = 50,
                  value = 1),
      radioButtons("tails", "P-value type",
                   choices = c("Left",
                               "Right",
                               "Both tails",
                               "Interior"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
 
  ###########################################################
  output$distPlot <- renderPlot({
    
    # draw t-distribution
    pdf <- data.frame(
      x = seq(-5,5,.01), 
      y = dt(seq(-5,5,.01), input$sample_size - 1))
    
    # what to shade
    if(input$tails == "Right") {
      pdf$shade <- pdf$y * (pdf$x > input$zscore)
      pvalue <- pt(input$zscore,input$sample_size - 1, lower.tail = FALSE )
    } else if(input$tails == "Left") {
      pdf$shade <- pdf$y * (pdf$x < input$zscore)
      pvalue <- pt(input$zscore,input$sample_size - 1, lower.tail = TRUE )
    } else if(input$tails == "Both tails") {
      pdf$shade <- pdf$y * (pdf$x < -abs(input$zscore) | pdf$x  > abs(input$zscore))
      pvalue <- pt(abs(input$zscore),input$sample_size - 1, lower.tail = FALSE)*2
    }  else { # inside
      pdf$shade <- pdf$y * (pdf$x > -abs(input$zscore) & pdf$x  < abs(input$zscore))
      pvalue <- 1 - pt(abs(input$zscore),input$sample_size - 1, lower.tail = FALSE)*2
      
    }
    
    # draw the histogram 
    ggplot(pdf, aes(x = x, y = y)) +
      geom_line() +
      geom_ribbon(aes(ymax=shade),ymin=0,
                  fill="blue",color=NA,alpha=0.5) +
      theme_bw() +
      geom_vline(xintercept = 0, color = "black", size = 1) +
      geom_vline(xintercept = input$zscore, color = "red", size = 1) +
      ggtitle(paste("p-value =", round(pvalue,4))) +
      theme(plot.title = element_text(size = 20, face = "bold"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
