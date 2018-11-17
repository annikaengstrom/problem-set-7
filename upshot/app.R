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
library(rsconnect)

# Read in the necessary data
df <- read_rds("data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Republican Advantage in Polls vs. Reality"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "State", 
                  label = "Select a state:",
                  choices = c("All", unique(df$State)),
                  selected = "All")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
      
      # h6("The line shows where poll and actual values were the same. Above it, and the actual Republican advantage was higher than polls predicted. Below it, and the polls predicted a higher advantage than actually happened."),
      # 
      # textOutput("selected_var"),
      # 
      # h6("Polling data from Upshot/Siena College and actual results from the Cook Political Report and the Associated Press.")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    # ifelse(input$State == "All", data <- df, data <- filter(df, State == input$State))
    
    ggplot(df, aes(`Percent (Race)`, Error, group = Race)) +
      geom_point(aes(col = Prediction)) +
      scale_color_manual(values=c("#009E73", "#f45642")) +
      geom_smooth(method = lm, se = FALSE) +
      facet_wrap(~ Race, ncol = 1) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, .2))
  }, height = 1400, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)
